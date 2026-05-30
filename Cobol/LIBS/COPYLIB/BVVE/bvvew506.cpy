      ******************************************************************
      **                                                              **
      **  FILE NAME:        CMQTMC2L                                  **
      **                                                              **
      **  DESCRIPTIVE NAME: COBOL COPY FILE FOR MQTMC2 STRUCTURE      **
      **                                                              **
      **  @START_COPYRIGHT@                                           **
      **  STATEMENT:     LICENSED MATERIALS - PROPERTY OF IBM         **
      **                                                              **
      **                 5695-137                                     **
      **                 (C) COPYRIGHT IBM CORPORATION. 1994, 1997    **
      **                                                              **
      **  STATUS:        VERSION 1 RELEASE 2                          **
      **                                                              **
      **  @END_COPYRIGHT@                                             **
      **                                                              **
      **  FUNCTION:         THIS FILE DECLARES THE MQTMC2 STRUCTURE,  **
      **                    WHICH IS PART OF THE IBM MESSAGE QUEUE    **
      **                    INTERFACE (MQI).                          **
      **                                                              **
      **  ENVIRONMENT:      MVS/ESA                                   **
      **                                                              **
      ******************************************************************

      **   MQTMC2 STRUCTURE
        10 MQTMC.
      **    STRUCTURE IDENTIFIER
         15 MQTMC-STRUCID     PIC X(4).
      **    STRUCTURE VERSION NUMBER
         15 MQTMC-VERSION     PIC X(4).
      **    NAME OF TRIGGERED QUEUE
         15 MQTMC-QNAME       PIC X(48).
      **    NAME OF PROCESS OBJECT
         15 MQTMC-PROCESSNAME PIC X(48).
      **    TRIGGER DATA
         15 MQTMC-TRIGGERDATA PIC X(64).
      **    APPLICATION TYPE
         15 MQTMC-APPLTYPE    PIC X(4).
      **    APPLICATION IDENTIFIER
         15 MQTMC-APPLID      PIC X(256).
      **    ENVIRONMENT DATA
         15 MQTMC-ENVDATA     PIC X(128).
      **    USER DATA
         15 MQTMC-USERDATA    PIC X(128).
      **    QUEUE MANAGER NAME
         15 MQTMC-QMGRNAME    PIC X(48).
