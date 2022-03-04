-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Galley.API.MLS (sendWelcome, sendLocalWelcome) where

import Control.Comonad
import Data.Bifunctor
import Data.Id
import Data.Qualified
import Data.Time
import Galley.API.Error
import Galley.API.Push
import Galley.Data.Conversation
import Galley.Effects.BrigAccess
import Galley.Effects.FederatorAccess
import Galley.Effects.GundeckAccess
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.ErrorDescription
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

sendWelcome ::
  Members
    '[ BrigAccess,
       FederatorAccess,
       GundeckAccess,
       Error UnknownWelcomeRecipient,
       Input UTCTime,
       Input (Local ())
     ]
    r =>
  Local UserId ->
  -- | Welcome message to forward.
  RawMLS Welcome ->
  Sem r ()
sendWelcome lusr wel = do
  (locals, remotes) <- welcomeRecipients lusr (extract wel)
  sendLocalWelcomeTo (rmRaw wel) locals
  sendRemoteWelcome wel remotes

sendLocalWelcome ::
  Members
    '[ BrigAccess,
       GundeckAccess,
       Error UnknownWelcomeRecipient,
       Input UTCTime
     ]
    r =>
  Local x ->
  RawMLS Welcome ->
  Sem r ()
sendLocalWelcome loc wel = do
  (locals, _) <- welcomeRecipients loc (extract wel)
  sendLocalWelcomeTo (rmRaw wel) locals

sendRemoteWelcome ::
  Members '[FederatorAccess] r =>
  RawMLS Welcome ->
  [Remote (UserId, ClientId)] ->
  Sem r ()
sendRemoteWelcome wel rcpts = do
  runFederatedConcurrently_ (fmap void rcpts) $ \_ ->
    void $ fedClient @'Galley @"mls-send-welcome" wel

welcomeRecipients ::
  Members
    '[ BrigAccess,
       Error UnknownWelcomeRecipient
     ]
    r =>
  Local x ->
  Welcome ->
  Sem r (Local [(UserId, ClientId)], [Remote (UserId, ClientId)])
welcomeRecipients loc =
  fmap (first (qualifyAs loc) . partitionQualified loc)
    . traverse (fmap cidQualifiedClient . derefKeyPackage . gsNewMember)
    . welSecrets

sendLocalWelcomeTo ::
  Members '[GundeckAccess, Input UTCTime] r =>
  ByteString ->
  Local [(UserId, ClientId)] ->
  Sem r ()
sendLocalWelcomeTo rawWelcome lclients = do
  now <- input
  -- TODO: add ConnId header to endpoint
  let mkPush u c =
        -- FUTUREWORK: use the conversation ID stored in the key package mapping table
        let lcnv = qualifyAs lclients (selfConv u)
            lusr = qualifyAs lclients u
            e = Event (qUntagged lcnv) (qUntagged lusr) now $ EdMLSMessage rawWelcome
         in newMessagePush lclients () Nothing defMessageMetadata (u, c) e
  runMessagePush lclients Nothing $
    foldMap (uncurry mkPush) (tUnqualified lclients)
  where

derefKeyPackage ::
  Members
    '[ BrigAccess,
       Error UnknownWelcomeRecipient
     ]
    r =>
  KeyPackageRef ->
  Sem r ClientIdentity
derefKeyPackage ref =
  maybe (throwED @UnknownWelcomeRecipient) pure
    =<< getClientByKeyPackageRef ref
