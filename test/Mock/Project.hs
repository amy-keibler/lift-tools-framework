{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
module Mock.Project where

import Lift.ToolIntegration.Project

import Test.HMock

import Relude

makeMockable ''MonadProject

expectLogging :: (MonadIO m, Typeable m) => MockT m ()
expectLogging = expectAny $ LogMessage_ anything anything
