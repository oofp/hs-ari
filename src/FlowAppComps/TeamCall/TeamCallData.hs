{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

--  cmd <- startFlowContext appContainer
--  let hndReq rq = cmd $ handleRequest rq
--  let sipAccount = AccountConfig "userName" "pwd" "domain" Nothing
--  hndReq $ SetSIPAccount sipAccount
--  let dialCfg = DialConfig "106" (Just "107") Nothing Nothing
--  hndReq $ SetDialConfig dialCfg
--  let mc= FlowAppComps.BasicCallApp.MakeCall (FlowAppComps.BasicCallApp.MakeCallParams "18001234567")
--  hndReq $ BCRequest mc

module FlowAppComps.TeamCall.TeamCallData
  where

import Protolude
import Elm.Derive
import Elm.Module

data MemberRole = Anchor
                | Participant
                | Guest
                deriving (Show, Eq, Generic)

data MemberData = MemberData
  { memberPhoneNum :: Text
  , memberName :: Text
  , memberRole :: MemberRole
  , memberClientData :: Text
  , memberDataExtData :: Maybe Int
  } deriving (Show, Eq, Generic)

data MeetingStartMode = StartManually
                      | AnyMember
                      | MustParticipant
                      | MustAnchor
                      deriving (Show, Eq, Generic)

data MeetingConfig = MeetingConfig
  { startMode :: MeetingStartMode
  , promptOnConnect :: Bool
  , recordName :: Bool
  , stopGraceTimeoutSec :: Integer
  , meetingConfigExtData :: Maybe Int
  } deriving (Show, Eq, Generic)

data MemberConnectedState = MemberConnectedState
                          { muted :: Bool
                          } deriving (Show, Eq, Generic)
data MemberState = MemberInitiated
                 | MemberRinging
                 | MemberAnswered
                 | MemberIntro
                 | MemberStandBy
                 | MemberConnected MemberConnectedState
                 | MemberOnHold
                 deriving (Show, Eq, Generic)

data MemberEntry = MemberEntry
                 { memberData :: MemberData
                 , memberState :: MemberState
                 } deriving (Show, Eq, Generic)

data MeetingState = MeetingStateNull
                  | MeetingStateInit
                  | MeetingStateActive
                  | MeetingStateGrace
                  deriving (Show, Eq, Generic)

type MembersMap = Map Text MemberEntry
type TalkingMembersSet = Set Text

data Members = Members (Map Text MemberEntry) deriving (Show, Eq, Generic)
data TalkingMembers = TalkingMembers (Set Text) deriving (Show, Eq, Generic)

data InitMeetingData = InitMeetingData
     { meetingMembers :: [MemberData]
     , meetingConfig :: MeetingConfig
     , startMeetingExtData :: Maybe Int
     } deriving (Show, Eq, Generic)

data MemberRequestType = ConnectMember
                       | HoldMember
                       | MuteMember
                       | UnmuteMember
                       | DropMember
                       deriving (Show, Eq, Generic)
data MemberRequestData = MemberRequestData
                       { memberID :: Text
                       , reqType :: MemberRequestType
                       } deriving (Show, Eq, Generic)

data TeamRequest = InitMeeting InitMeetingData
                 | InviteMember MemberData
                 | MemberRequest MemberRequestData
                 | StartMeeting
                 | StopMeeting
                 | GetAllMembers
                 deriving (Show, Eq, Generic)

data TeamEvent = MembersEvent Members
               | TalkingEvent TalkingMembers
               | MeetingState MeetingState
               deriving (Show, Eq, Generic)

----------------------------------------
deriveBoth defaultOptions ''MemberRole
deriveBoth defaultOptions ''MemberData
deriveBoth defaultOptions ''MeetingConfig
deriveBoth defaultOptions ''MeetingStartMode
deriveBoth defaultOptions ''MemberConnectedState
deriveBoth defaultOptions ''MemberState
deriveBoth defaultOptions ''MemberEntry
deriveBoth defaultOptions ''Members
deriveBoth defaultOptions ''TalkingMembers
deriveBoth defaultOptions ''MemberRequestType
deriveBoth defaultOptions ''MemberRequestData
deriveBoth defaultOptions ''InitMeetingData
deriveBoth defaultOptions ''TeamRequest
deriveBoth defaultOptions ''MeetingState
deriveBoth defaultOptions ''TeamEvent

printTeamData :: IO ()
printTeamData =  putStrLn $ makeElmModule "TeamCalllData"
    [ DefineElm (Proxy :: Proxy MemberRole)
    , DefineElm (Proxy :: Proxy MemberData)
    , DefineElm (Proxy :: Proxy MeetingStartMode)
    , DefineElm (Proxy :: Proxy MeetingState)
    , DefineElm (Proxy :: Proxy MeetingConfig)
    , DefineElm (Proxy :: Proxy MemberConnectedState)
    , DefineElm (Proxy :: Proxy MemberState)
    , DefineElm (Proxy :: Proxy MemberEntry)
    , DefineElm (Proxy :: Proxy Members)
    , DefineElm (Proxy :: Proxy TalkingMembers)
    , DefineElm (Proxy :: Proxy MemberRequestType)
    , DefineElm (Proxy :: Proxy MemberRequestData)
    , DefineElm (Proxy :: Proxy InitMeetingData)
    , DefineElm (Proxy :: Proxy TeamRequest)
    , DefineElm (Proxy :: Proxy TeamEvent)
    ]
---------------------------------------
