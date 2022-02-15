{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.Login_user where

import Data.Handle (Handle (Handle, fromHandle))
import Data.Misc (PlainTextPassword (PlainTextPassword))
import Imports (Maybe (Just, Nothing))
import Wire.API.User (Email (Email, emailDomain, emailLocal), Phone (Phone, fromPhone))
import Wire.API.User.Activation (ActivationCode (..))
import Wire.API.User.Auth
  ( CookieLabel (CookieLabel, cookieLabelText),
    Login (..),
    LoginCode (LoginCode, fromLoginCode),
    LoginId (LoginByEmail, LoginByHandle, LoginByPhone),
  )

testObject_Login_user_1 :: Login
testObject_Login_user_1 =
  PasswordLogin
    (LoginByEmail (Email {emailLocal = "4\1069339\vEaP", emailDomain = "\ENQ\n\FS\ESC\997356i03!"}))
    ( PlainTextPassword
        "\b5Ta\61971\150647\186716fa&\1047748o!ov\SI\1100133i\DC4\ETXY\SOR\991323\1086159Ta^s\ETB\SI[\189068\988899\26508\CAN6\STXp\1069462-9\983823&\NAK\1052068]^\13044;>-Z$Z\NAK\r\1101550a\RS%\NUL:\188721\47674\157548?e]\ETX \142608 C\SOH\SIS%8m\1091987V\147131[\1006262\&6\171610\1011219\164656SX\n%\1061259*>\t+\132427Y\989558\993346\GSU\1067541\&6TU!*\40114\&90\1055516\RSV\162483N\t*\EOT{I<\1084278\SOH\183116!c\\\n\1107501\183146\DC1,-xX\EMV?\t\168648\1054239\DC2\DEL1\SOHu\SOH\63459\53061\SO+h\ACK::\RS\21356_g,\SO*\v\DC4\1093710HFF\188918\1081075fF\ESC2\SOHT\DC1)\fc\35905l\1061547\f#~\STX]\1035086/Or)kY\1031423\SOHNCk\1067954\&5\1083470x=H\NUL\23760\1058646\1099097E/$\DELpbi\137522\FSKi\15676\1018134\t7\"OL\54208\7516\&5\43466\NUL(\1030852\166514\SOH\149343\994835\25513C==\GSTV3\DELl6\999006.Z)$\16723|\172732\1090303J;O\GSbw\vI\1101024I\SYN\DC2^\149630\STX3%i\EMW\138614\DC4\1113619tsL5\147087W\96700(_,\1091179*\1041287rckx\SOH\SIs\SOHJd\140574\SYNev.\DC4\DLE\99082.\1106785\996992\143448\US_\ETBf\STX\SO\DC3\1043748\&6O\DC1Q\SOH'\GS,|]W\SIa\62568\151062.\v\aH&-L\DC2+\147179\1095524\EOTm)\19925\181147\183368!\185223\142946m\DC4\DC3\1034282m\GS\185509>>\"NDw\1076877hY\1033831sFKz^ \1108187\&5Qec\NAK}|\1108194.Q\173114imb\1027220 p;\1089082\SYN\1065748kF\1102854r8o\DC1"
    )
    (Just (CookieLabel {cookieLabelText = "r"}))
    Nothing

testObject_Login_user_2 :: Login
testObject_Login_user_2 =
  SmsLogin
    (Phone {fromPhone = "+956057641851"})
    (LoginCode {fromLoginCode = "\nG\1076650\&8\b"})
    (Just (CookieLabel {cookieLabelText = "G"}))

testObject_Login_user_3 :: Login
testObject_Login_user_3 =
  PasswordLogin
    (LoginByHandle (Handle {fromHandle = "c2wp.7s5."}))
    ( PlainTextPassword
        "&\RS\DC4\1104052Z\11418n\SO\158691\1010906/\127253<X\US\171995\SUBa\a\ETX\DLEP\1018713\NAK4B\a\41549\183051\&9(\GS\50200\988284\ENQ\SOH\128653Lll\nU\RSS\1007294r-X<\1006885\1053047Y\135696 @\58553\&9\1036718Sl\42785\1065539V\NAKC:e\EM\150993\SUB1i\EOTM\GSW\1026646\SUB1\n\990213\&3T0v\DC2'\1008594\182668I\SI\ETX\DLE\1081008o>'\1063038m\1010345\"\9772\138717\RS(&\996590\SOf1Wf'I\SI\100286\1047270\1033961\DC1Jq\1050673Y\\Bedu@\1014647c\1003986D\53211\1050614S\144414\ETX\ETXW>\1005358\DC4\rSO8FXy\166833a\EM\170017\SUBNF\158145L\RS$5\NULk\RSz*s\148780\157980\v\175417\"SY\DEL\STX\994691\1103514ub5q\ENQ\1014299\vN.\t\183536:l\1105396\RS\1027721\a\168001\SO\vt\1098704W\SYN\1042396\1109979\a'v\ETB\64211\NAK\59538\STX \NAK\STX\49684,\1111630x\1047668^\1067127\27366I;\NAKb\1092049o\162763_\190546MME\1022528\SI\1096252H;\SO\ETBs\SO\1065937{Knlrd;\35750\DC4\SI\1075008TO\1090529\999639U\48787\1099927t\1068680^y\17268u$\DC1Jp\1054308\164905\164446\STX\"\1095399*\SO\1004302\32166\990924X\1098844\ETXsK}\b\143918\NUL0\988724\&12\171116\tM052\189551\EOT0\RS\986138\1084688{ji\ESC\1020800\27259&t \SI\ESCy\aL\136111\131558\994027\r\1054821ga,\DC4do,tx[I&\DC4h\DLE\ETX\DLEBpm\1002292-\a]/ZI\1033117q]w3n\46911e\23692kYo5\1090844'K\1089820}v\146759;\1018792\\=\41264\&8g\DLEg*has\44159\1006118\DC3\USYg?I\19462\NAKaW2\150415m\t}h\155161RbU\STX\ETBlz2!\DC3JW5\ESC\1026156U\SOg,rpO\5857]0\ESC\479\1005443F\SI\1045994\RS\SO\11908rl\1104306~\ACK+Mn{5\993784a\EM2\v{jM\ETBT\1058105$\DC1\1099974\GSj_~Z\1007141P\SOH\EOTo@TJhk\EOT\ETBk:-\96583[p\DLE\DC1\RS'\r\STXQ,,\1016866?H\rh\30225\rj\147982\DC2\\(u\ESCu\154705\1002696o\DC4\988492\1103465\1052034\DC1q\GS-\b\40807\DC1qW>\fys\8130,'\159954<"
    )
    (Just (CookieLabel {cookieLabelText = "\1082362\66362>XC"}))
    (Just (ActivationCode "123456"))

testObject_Login_user_4 :: Login
testObject_Login_user_4 =
  SmsLogin
    (Phone {fromPhone = "+04332691687649"})
    (LoginCode {fromLoginCode = "\94770m"})
    (Just (CookieLabel {cookieLabelText = ":"}))

testObject_Login_user_5 :: Login
testObject_Login_user_5 =
  PasswordLogin
    ( LoginByHandle
        ( Handle
            { fromHandle =
                "c372iaa_v5onjcck67rlzq4dn5_oxhtx7dpx7v82lp1rhx0e97i26--8r3c6k773bxtlzmkjc20-11_047ydua_o9_5u4sll_fl3ng_0sa."
            }
        )
    )
    ( PlainTextPassword
        "\120347\184756DU\1035832hp\1006715t~\DC2\SOH\STX*\1053210y1\1078382H\173223{e\\S\SO?c_7\t\DC4X\135187\&6\172722E\100168j\SUB\t\SYN\1088511>HO]60\990035\ETX\"+w,t\1066040\ak(b%u\151197`>b\1028272e\ACKc\151393\1107996)\12375\&7\1082464`\186313yO+v%\1033664\rc<\65764\&2>8u\1094258\1080669\1113623\75033a\179193\NAK=\EOT\1077021\&8R&j\1042630\ESC\t4sj-\991835\40404n\136765\1064089N\GS\\\1026123\72288\&5\r\97004(P!\DEL\29235\26855\b\1067772Mr~\65123\EMjt>Z\GS~\140732A\1031358\SO\\>\DC16\">%\45860\1084751I@u5\187891\vrY\r;7\1071052#\1078407\1016286\CAN'\63315\1041397\EM_I_zY\987300\149441\EMd\1039844cd\DEL\1061999\136326Cp3\26325\GSXj\n\46305jy\44050\58825\t-\19065\43336d\1046547L\SUBYF\ACKPOL\54766\DC2\DC1\DC1\DC2*\r<X]\986083\97081\CANP\CANI3r\168316\SOc`.\134301'4\EOT\32219\39187\35064\1088605>H\DLE(?\DC3F\25820\DLE\r]\1069451j\170177 @\ENQT\1100685s\FSF2\NAK]8\a\DC3!\NAKW\176469\1110834K\1025058\1112222_%\1001818\1113069'\1098149\70360(#\SOHky\t\ETB!\17570\NAK\DC4\ESC{\119317U2LS'"
    )
    (Just (CookieLabel {cookieLabelText = "LGz%\119949j\f\RS/\SOH"}))
    (Just (ActivationCode "123456"))

testObject_Login_user_6 :: Login
testObject_Login_user_6 =
  PasswordLogin
    (LoginByPhone (Phone {fromPhone = "+930266260693371"}))
    ( PlainTextPassword
        "K?)V\148106}_\185335\1060952\fJ3!\986581\1062221\51615\166583\1071064\a\1015675\SOH7\\#z9\133503\1081163\985690\1041362\EM\DC3\156174'\r)~Ke9+\175606\175778\994126M\1099049\"h\SOHTh\EOT`;\ACK\1093024\ENQ\1026474'e{\FSv\40757\US\143355*\16236\1076902\52767:E]:R\1093823K}l\1111648Y\51665\1049318S~\EOT#T\1029316\&1hIWn\v`\45455Kb~\ESC\DLEdT\FS\SI\1092141f\ETBY7\DEL\RS\131804\t\998971\13414\48242\GSG\DC3BH#\DEL\\RAd\166099g\1072356\1054332\SIk&\STXE\22217\FS\FS\FS$t\1001957:O\1098769q}_\1039296.\SOH\DC4\STX\157262c`L>\1050744l\1086722m'BtB5\1003280,t\"\1066340\&9(#\ENQ4\SIIy>\1031158\1100542\GSbf\"i\ETB\14367a\1086113C@\1078844\1092137\32415\NAK\999161\23344*N\SYN\ESC:iXibA\136851\169508q\1048663]:9r\63027\73801\NUL\1050763\USCN\US\147710\1048697\1016861eR\RSZbD5!8N\ESCV\7344\ACK\173064\SUBuz\1053950\188308~\ESC\SI%{3I/F\25232/DMS\US>o\187199\63000Z\1108766\GS[K\184801\94661\1088369\995346\ESCO-4\CAN\US\FSZp"
    )
    (Just (CookieLabel {cookieLabelText = "\1014596'\998013KW\\\NUL\DC4"}))
    (Just (ActivationCode "123456"))

testObject_Login_user_7 :: Login
testObject_Login_user_7 =
  PasswordLogin
    (LoginByEmail (Email {emailLocal = "BG", emailDomain = "\12137c\v}\SIL$_"}))
    ( PlainTextPassword
        "&\991818\1023244\83352\STXJ<-~\STX>\v\74228\151871\&5QN\53968\166184ql\NAK\74290\&3}{\DC3\173242S\22739;\t7\183958_F~D*f\1049940)\1067330-9\20699\&7GK= %\RS@kOF#\179945\1094401\124994\&8_\42309\GSL\37698\ETX\1047946\&0Wl1A`LYz\USy\20728\SUBo\ESC[\DC4\bt\66640a\ETXs~\USF\175140G`$\vG\DC1\1044421\128611/\1014458C>\SI"
    )
    (Just (CookieLabel {cookieLabelText = "\SO\NAKeC/"}))
    (Just (ActivationCode "123456"))

testObject_Login_user_8 :: Login
testObject_Login_user_8 =
  PasswordLogin
    (LoginByEmail (Email {emailLocal = "", emailDomain = "~^G\1075856\\"}))
    ( PlainTextPassword
        "z>\1088515\1024903/\137135\1092812\b%$\1037736\143620:}\t\CAN\1058585\1044157)\12957\1005180s\1006270\CAN}\40034\EM[\41342\vX#VG,df4\141493\&8m5\46365OTK\144460\37582\DEL\44719\9670Z\"ZS\ESCms|[Q%\1088673\ENQW\\\1000857C\185096+\1070458\4114\17825v\180321\41886){\1028513\DEL\143570f\187156}:X-\b2N\EM\USl\127906\49608Y\1071393\1012763r2.1\49912\EOT+\137561\DC3\145480]'\1028275s\997684\42805.}\185059o\992118X\132901\11013\r\SUBNq6\1019605'\fd\RS\14503\1097628,:%\t\151916\73955QD\1086880\ESC(q4KDQ2zcI\DLE>\EM5\993596\&1\fBkd\DC3\ACK:F:\EOT\100901\11650O N\FS,N\1054390\1000247[h\DEL9\5932:xZ=\f\1085312\DC3u\RS\fe#\SUB^$lkx\32804 \rr\SUBJ\1013606\1017057\FSR][_5\NAK\58351\11748\35779\&5\24821\1055669\996852\37445K!\1052768eRR%\32108+h~1\993198\35871lTzS$\DLE\1060275\"*\1086839pmRE\DC3(\US^\8047Jc\10129\1071815i\n+G$|\993993\156283g\FS\fgU3Y\119068\ACKf)\1093562\SYN\78340\1100638/\NULPi\43622{\1048095j\1083269\FS9\132797\1024684\32713w$\45599\126246)Si\167172\29311FX\1057490j{`\44452`\999383\159809\&4u%\1070378P*\1057403\25422\DELC\RSR\SYN-\51098\1011541g\68666:S>c\15266\132940\DLEY\1066831~a)YW_J\1063076P\a+ U\1084883j\EMk\SOH\1096984\DC1\18679e\172760\175328,\5135g@\DC2\GSHXl.\ETB\153793\&2\DC3mY\1054891\tv?L8L\1074044N\133565\nb1j\1044024\148213xfQ=\\\ENQe\995818\1023862U\DC2p{\SO\1099404jd^@U\994269tP.\DC2Y%R`a\r\160622\&7}HnUf\132856m^7:\NAK=\52348>l\95313hwp27\149950jE\fx=!.\DC3]Ar\tw\DC4&\SUBk\194572s\1042820\4498I\146071\61461\1060645dsY\DLE\181922dX.\146295i]\151113\1028288\rWS\USU\1098732\SUB\49884\1083906\DLE\STXN~-\SO6\190031\1110322\\O\185165Jc\1052359\1071278\NULHSo\DLE-W\DC36\170321I\1068712)\99800={\99796h\27961\61707M\1022570FwJQ\1111976ck\SUB\CAN|UV-\NAK\SOH|\DC4;\f\156907\145795\ENQS\NAK.B\"D\163007#o*\126577\32988m\RS\1049834B3Gg;\DC1\\\180659\1098926\ENQ B^\SI\152630$e\39220\170037>fMgC\187276,o\128488\\?\1033955~/s\SOH?MMc;D18Ne\EOT\CAN)*\STX\GS\16268<r1IWrG\195059\145938\9725\&32\bl4\1079274\190780\&3'\153484-8d\"\RSe\26305|n*a\DC1\GS\ACKQ\191157\nKu4k'\1000945\1056819NokNg\RSZu\1060680\&9\141344{JoE\1020205\1032779J5ln\44292C1`7\EM#3\1066049oh\160813?nN\996073vZPR7g\1048725;lF|wC\141456RR85\vVC\EMy%\ETXLjT\175980\SI@X\152574\16984_6\137913\110604i\49958\146144\1027378$@y\71249t\FS\SOyZ\EMA4\48493l0\DLE\SYNr4\GS-\NAK~\SO]=g.\SO\185464\72285\1006719\1066773\DC2\NUL\184316\STX-n\SOHm\165115_;\172002\1109775\43334\1100332W\SOHr\SYNXGC\1004793\vF\SI\129141\SUB\NUL~^~9e\1011753gv\RS\CAN\1073867W[VJ(\\\58715\37252Tp3\ESCz\b\DC1h5\1008688\120915@kL?Gh!\buMH*m\SOH<\187785\n!X?eV\RSmMt\SUB\50146\1077422V\GS\r*{o<\STXu\984721\SOH.5\US\DLE\993739\DC2\1082268\1067311%80'\83369HJU"
    )
    Nothing
    (Just (ActivationCode "123456"))

testObject_Login_user_9 :: Login
testObject_Login_user_9 =
  PasswordLogin
    (LoginByHandle (Handle {fromHandle = "6bolp"}))
    ( PlainTextPassword
        ">1/\t\NAK \1010386\1013311z\33488Bv\1109131(=<\SOq\1104556?L\6845\1066491\2972c\997644<&!\1103500\999823j~O3U<tW\178757\992380\191046yz\157855\1076820\DEL\n\DC2\ff\r\12623\153520\143219O\15243)\1014051D\f\\\\:\t\133548\CAN\SI\1093232\&4&V\28139\1762B\1048934\111001\1065099\ETX\DC2\v\993760\rK)\1081426\SUB\DLE\49495\SIv\1001778~!YS|\16107\&41Je+\986824\1103501\991761AMa7%1\1073046\1002102\ETBl\CAN\DC2'\1042831/j\SO\FSp\178875n:\4266\RSI\ENQ\EOT\64782WU`\20527\\\19018\161873\US>Sw\DC2\ETX\a\ETB+\1024033Ny\31920(/Sco\STX{3\SIEh\SYN\1032591\1022672\27668-\FS.'\ENQX\98936\150419Ti3\1051250\"%\SYN\b\188444+\EOT\STX^\1108463)2bR\ACK\SIJB[\1045179&O9{w{aV\ENQgZ?3z\1065517\&8\4979\156950\990517`\1063252\"PE)uKq|w\SYN0\ESC. \ETX\73440sxW\160357\1001111m\ENQ7e)\77912\1008764:s\CANYj\9870\16356\ACK\USlTu\1110309I.\1087068O#kQ\RS!g\1062167\CANQ\US\172867\SYN\ACK|\"M\"P\US\ETX@ZPq\1016598gY\148621=\a\1057645l8\1041152\&3\995012\1022626CN<\147876gJ\1038434]\94932mX~\ACKw3\DLE\179764\&8\a6\EOT}\DLEi\DC3L5\1032336PY^|!Vz\ESC4\36208!iLa\12091\DC4\1059706\167964\GS:\1042431\149640h\\dLx\1087701\EM\194900\SUB\134635R%ps7\95168s\1074387fg\nIf\1067199\DC1l\SUB\1022871-n_\6065UY?4d]|c\\[T\ajS\18838\55046\37136aK\1025430\1112672\ETX\FSx+"
    )
    (Just (CookieLabel {cookieLabelText = ""}))
    (Just (ActivationCode "123456"))

testObject_Login_user_10 :: Login
testObject_Login_user_10 =
  SmsLogin
    (Phone {fromPhone = "+4211134144507"})
    (LoginCode {fromLoginCode = "\13379\61834\135400!\ETBi\1050047"})
    (Just (CookieLabel {cookieLabelText = ""}))

testObject_Login_user_11 :: Login
testObject_Login_user_11 =
  SmsLogin (Phone {fromPhone = "+338932197597737"}) (LoginCode {fromLoginCode = "\1069411+W\EM3"}) Nothing

testObject_Login_user_12 :: Login
testObject_Login_user_12 =
  PasswordLogin
    (LoginByPhone (Phone {fromPhone = "+153353668"}))
    ( PlainTextPassword
        "n\1095465Q\169408\ESC\1003840&Q/\rd\43034\US\EOTw2C\ACK\1056364\178004\EOT\EOTv\1010012\bf,b\DEL\STX\1013552'\175696C]G\46305\1017071\190782\&4\NULY.\173618\SO3sI\194978F\1084606\&5\21073rG/:\"\1013990X\46943\&6\FS:\CAN\aeYwWT\1083802\136913Msbm\NAK@\984540\1013513\EOT^\FS\147032\NAK@\ENQ>\f\RSUc\EOTV9&c\3517\a\986228a'PPG\100445\179638>[\3453\&2\64964Xc\131306[0\1002646\b\99652B\DC1[\1029237\GS\19515\US\EMs-u\ETBs\1067133\1005008\161663n\1072320?\1045643ck\DC48XC\174289\RSI2\2862\STX\DLEM\ESC\n?<\\\DC3E\72219\GS\n$cyS\136198!,\v9\ETB/\DC1\62324?P\ETB\41758\DC2\999537~\1058761W-W4K8.\DC27\EML\1078049h\SI}t+H\SUB\ESCX\120523s\EOTt\177703taa\GS\f\152365(v\1024552M\ESCvg3P1\1032835\57603]g\3933\&4T\NAK$\38212);\\8\1109165\nK\NAK}D'^fJ'\143205e\174052\39597!\EM.\DC2{\\CEp\1045384\ETBk_\1083904\18397\164138\1063468]MG$\187650[E\1112126\b\1073487{b\50650\ESC^b@W\NAK$\FS<\1023895&\155992R\ACKJ\SI\1093108\1101041\41438n\1007134\&8]\148288\ENQ}|k\STX\CANQ\USI\a\CANDZ\1062877\NUL\50197rb\18947\&3G%\FS\162081\EOT\NAK4YB0-i\1018065IM\1073908[\1111554:Cr$\99636)L\136837W\40897.x;\41461\1030711\995525\USkb\CANY9)\SYN4\SI\1103461Av.\r\f\1061861\&9{\SO\ETBP\f\33538u\r-9cB4\1016091G\RS\22817\1014740r\128247HcsPm\59419s\120987!|J<\DLE8\FS[\NAKWYAK\75011^\987050c3\1042176\aC\ETX\ETB\1053739Y\DC4f\ACK\1060945!\1032209:RlQ!BX\f=\1070694f\151362\DEL\113727O\ETX\\\"\53275B<\RSLV4g%3\1098063\ACK`\NAK>\n\44626kp\986102\171479\DEL\60526H\20888lyJ\DC2)\1055149(\1027099A\FSh\EOTj\35251\DC4M\ESCP-q\bn\CAN\143310~\GS\EM\"o\21512%*e2\165597L\1023807sy\152913\&2m\GS\1049046{EG]\DC16B+{\983622IYa\1008153\&5,<\ESCX\f\SI\186613\153744E\134407\1011088L<\EMdUO\ETB\SUBZYm\ACK\1086320R\SUB\991954\DC3^\60967s\fu_g\EM?i~}\DELV2\148681R\FS\EOT3j\45841m\1542\1100884\n7S\SIT5j\170914\SI\1015133\141587h\182480Q\146618\59914\DEL\NAKZM\1110574\&02f\129340l!*\SOH\1027033\SOH\1070384\1094775\t\72805\ESCa:q UKEN\RS-\n\ETXH\22365a\1074707\b\37494\"\1035508\149695\1033139R4\ETX\DLE\FS\STX\1004750%\"@\1009369\&6=/x\NULP\EOT\174871/\190041\f\f\1005146?<T9\fI,\30219\DC4l#\136541\10462_\111051\SOH\ETBK?\ETXQ\GSh\1055028C\r\US<}\38479a?4^\DC4X\"w!\1051144 N'\EM,=\DC45p\1048996Ur\164033\1001175\119297y\1046287 \78720\&8\ESC\STX \26408KbY-\DC3\ETB{c\DLEy\tl\60972\SOH>*\fIcKW\DELQ\"\1001726P*\1095849\&6=d\n\157680\RS\1087962\EOT\DC2I\47501U\b=Pc\DLE"
    )
    (Just (CookieLabel {cookieLabelText = "\SI\128787-\125004:\136001\39864\ACK\SO"}))
    (Just (ActivationCode "123456"))

testObject_Login_user_13 :: Login
testObject_Login_user_13 =
  SmsLogin (Phone {fromPhone = "+626804710"}) (LoginCode {fromLoginCode = "&\1040514y"}) Nothing

testObject_Login_user_14 :: Login
testObject_Login_user_14 =
  SmsLogin
    (Phone {fromPhone = "+5693913858477"})
    (LoginCode {fromLoginCode = ""})
    (Just (CookieLabel {cookieLabelText = "\95804\25610"}))

testObject_Login_user_15 :: Login
testObject_Login_user_15 =
  SmsLogin
    (Phone {fromPhone = "+56208262"})
    (LoginCode {fromLoginCode = ""})
    (Just (CookieLabel {cookieLabelText = "q\ETB(\1086676\187384>8\141442\n6"}))

testObject_Login_user_16 :: Login
testObject_Login_user_16 =
  SmsLogin
    (Phone {fromPhone = "+588058222975"})
    (LoginCode {fromLoginCode = "_\1110666\1003968\1108501-_\ETB"})
    (Just (CookieLabel {cookieLabelText = "\SOL\1079080\1008939\1059848@\FS\DLE$"}))

testObject_Login_user_17 :: Login
testObject_Login_user_17 =
  SmsLogin
    (Phone {fromPhone = "+3649176551364"})
    (LoginCode {fromLoginCode = "\ETB1\1002982n\DLEdV\1030538d\SOH"})
    (Just (CookieLabel {cookieLabelText = "\1112281{/p\100214"}))

testObject_Login_user_18 :: Login
testObject_Login_user_18 =
  SmsLogin
    (Phone {fromPhone = "+478931600"})
    (LoginCode {fromLoginCode = ",\139681\13742,"})
    (Just (CookieLabel {cookieLabelText = "5"}))

testObject_Login_user_19 :: Login
testObject_Login_user_19 =
  SmsLogin
    (Phone {fromPhone = "+92676996582869"})
    (LoginCode {fromLoginCode = "x\27255<"})
    (Just (CookieLabel {cookieLabelText = "w;U\ESCx:"}))

testObject_Login_user_20 :: Login
testObject_Login_user_20 =
  PasswordLogin
    (LoginByEmail (Email {emailLocal = "[%", emailDomain = ","}))
    ( PlainTextPassword
        "ryzP\DC39\11027-1A)<r\r\1056828\ENQ+R\18109]rwWyPau\6948\1062863\&8S\ETB\n\178919g\ACK:\SI\FS%nm1]\NUL\1050293\ETB\23682):x\95186\16646\190157\DLE\1026833'\US\ESC\999436Ag\DEL\994984;l>\b,u\8457j~0\1090580\1033743\fI\170254er\DC4V|}'kzG%A;3H\amD\STXU1\NUL^\1043764\DLEO&5u\EOT\SUB\167046\&0A\996223X\DC2\FS7fEt\97366rPvytT\136915!\100713$Q|BI+EM5\NAK\t\DELRKrE\DLE\US\r?.\STX|@1v^\vycpu\n$\DC2\186675\131718-Q\151081\n\r\1033981\68381O\ENQ*\68660Z\USo\EOTn\188565%&\DC3Me*\STX;\DLE034\nv\NAK\140398(\1075494\990138n@\1108345|\48421d\n*\SI\NUL}\NAKA!\1045882\1036527Hx\ETB3\STX{#T|5|GC\1089070z.\USN\1080851\22324\vu\SYN~LP\147583CV\SO q\151952\DC2e8h\USg\1019358;\f\996107\1108688At\1022346)\USG\DC3\166541\39337|\1042043\SI\134073\EOTc~6\DLE:u\165393##^\nn{d\CAN\ng\16237\ESC\US\US~A8};T\RS\NAK)&\b\ACK\1106044\GS(\DC3u;\1094683;=e\1051162\"\40669vCt)o\987006m\43912\78088l1+\1036284[\STXFLx\1080932:\1031973\992752\&71/kE\93787p\DC4Ij\ETB\194985&\SUB^\FSl1\ACK\1019548\ETXW,+3\128058\95671\DLE7\59727\&7rG'\1078914JC9M\1053804\SYN\DC2\44350>~\1016308Y\1062059=i-\fS\172440\156520K2-@\ENQ\f\1108851_1D-&\128386lR\187248/\993988$:\31415:\52267Dg\1015243O\1010173\170117\SO\179807\&2z\NAKq\141547c\FSliJ{\1055925\1060070'BL\168670;\STX\1046844\18443B\NUL\7839b\1072569:w\1108016Ad\SUB6\NAKo\55279\nsPWM{\ETXfW\1018373JT\1021361$\989069\54608\190318\173259u4\1103286\t\34021\1039458\"\153264UM\1084148\1095406\34105\1105325\t\nIn'\1070532\21097\16091\EM\DC1<\v\bW\SI}\141807\b\1072339\1035283\GS`\1094467x\NUL\986937K\FSj\1079287\DC1\SI\168992d\991620k4\SUB\1009876\49943^\58464\1052547\1016875i2=$:[f\1064579\DC2n\NAKJ<=\2028\SI!z\1105364\SON\NAK\EM\180748V\1024876CQ_G\nY#ky\132779k\DC3\ENQ}OC\96566}~M\EMp\ETX\RSx\b\183962\1073008\b8/\DC4?\1081654B\1025870\EOT\SO\DELU\1020905\ESC=%\51062J\168855\ETB\992593\990312\985186\to\1101036X_@@\45111\43952$"
    )
    (Just (CookieLabel {cookieLabelText = "\1055424\r9\998420`\NAKx"}))
    (Just (ActivationCode "123456"))
