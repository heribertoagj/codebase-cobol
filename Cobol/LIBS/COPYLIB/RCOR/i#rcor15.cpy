      ******************************************************************
      **                                                              **
      **                  WebSphere MQ for z/OS                       **
      **                                                              **
      **  FILE NAME:      CMQTMC2L                                    **
      **                                                              **
      **  DESCRIPTION:    Trigger Message 2 (Character) Structure     **
      **                                                              **
      ******************************************************************
      **  @START_COPYRIGHT@                                           **
      **  Statement:     Licensed Materials - Property of IBM         **
      **                                                              **
      **                 5655-F10                                     **
      **                 (C) Copyright IBM Corporation. 1993, 2002    **
      **                                                              **
      **  Status:        Version 5 Release 3                          **
      **  @END_COPYRIGHT@                                             **
      ******************************************************************
      **                                                              **
      **  FUNCTION:       This file declares the structure MQTMC2,    **
      **                  which is used by the main MQI.              **
      **                                                              **
      **  PROCESSOR:      COBOL                                       **
      **                                                              **
      ******************************************************************



      ******************************************************************
      **  MQTMC2 Structure -- Trigger Message 2 (Character)           **
      ******************************************************************

      ** MQTMC2 structure
       10  MQTMC.
      ** Structure identifier
       15  MQTMC-STRUCID PIC X(4).
      ** Structure version number
       15  MQTMC-VERSION PIC X(4).
      ** Name of triggered queue
       15  MQTMC-QNAME PIC X(48).
      ** Name of process object
       15  MQTMC-PROCESSNAME PIC X(48).
      ** Trigger data
       15  MQTMC-TRIGGERDATA PIC X(64).
      ** Application type
       15  MQTMC-APPLTYPE PIC X(4).
      ** Application identifier
       15  MQTMC-APPLID PIC X(256).
      ** Environment data
       15  MQTMC-ENVDATA PIC X(128).
      ** User data
       15  MQTMC-USERDATA PIC X(128).
      ** Queue manager name
       15  MQTMC-QMGRNAME PIC X(48).

      ******************************************************************
      **  End of CMQTMC2L
      ******************************************************************
