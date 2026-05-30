      ***===========================================================***
      *** NOME INC                                     LENGTH=0100  ***
      *** I#SLIG00                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO DE DATAS                               ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-DATAS.
           05 SLIG00-CD-ID-PRODUTO        PIC 9(002).
           05 SLIG00-CD-TP-PROCESSA       PIC 9(001).
           05 SLIG00-DT-PROCE-ATUAL       PIC 9(008).
           05 SLIG00-DT-PROCE-ATUAL-R REDEFINES SLIG00-DT-PROCE-ATUAL.
              10 SLIG00-ANO-PROCE-ATUAL   PIC 9(004).
              10 SLIG00-MES-PROCE-ATUAL   PIC 9(002).
              10 SLIG00-DIA-PROCE-ATUAL   PIC 9(002).
           05 SLIG00-DT-PROCE-ULTIM       PIC 9(008).
           05 SLIG00-DT-PROCE-ULTIM-R REDEFINES SLIG00-DT-PROCE-ULTIM.
              10 SLIG00-ANO-PROCE-ULTIM   PIC 9(004).
              10 SLIG00-MES-PROCE-ULTIM   PIC 9(002).
              10 SLIG00-DIA-PROCE-ULTIM   PIC 9(002).
           05 SLIG00-DT-PROCE-PROXI       PIC 9(008).
           05 SLIG00-DT-PROCE-PROXI-R REDEFINES SLIG00-DT-PROCE-PROXI.
              10 SLIG00-ANO-PROCE-PROXI   PIC 9(004).
              10 SLIG00-MES-PROCE-PROXI   PIC 9(002).
              10 SLIG00-DIA-PROCE-PROXI   PIC 9(002).
           05 SLIG00-DT-INI-PER-ATU       PIC 9(008).
           05 SLIG00-DT-INI-PER-ATU-R REDEFINES SLIG00-DT-INI-PER-ATU.
              10 SLIG00-ANO-INI-PER-ATU   PIC 9(004).
              10 SLIG00-MES-INI-PER-ATU   PIC 9(002).
              10 SLIG00-DIA-INI-PER-ATU   PIC 9(002).
           05 SLIG00-DT-FIM-PER-ATU       PIC 9(008).
           05 SLIG00-DT-FIM-PER-ATU-R REDEFINES SLIG00-DT-FIM-PER-ATU.
              10 SLIG00-ANO-FIM-PER-ATU   PIC 9(004).
              10 SLIG00-MES-FIM-PER-ATU   PIC 9(002).
              10 SLIG00-DIA-FIM-PER-ATU   PIC 9(002).
           05 SLIG00-DT-INI-PER-ULT       PIC 9(008).
           05 SLIG00-DT-INI-PER-ULT-R REDEFINES SLIG00-DT-INI-PER-ULT.
              10 SLIG00-ANO-INI-PER-ULT   PIC 9(004).
              10 SLIG00-MES-INI-PER-ULT   PIC 9(002).
              10 SLIG00-DIA-INI-PER-ULT   PIC 9(002).
           05 SLIG00-DT-FIM-PER-ULT       PIC 9(008).
           05 SLIG00-DT-FIM-PER-ULT-R REDEFINES SLIG00-DT-FIM-PER-ULT.
              10 SLIG00-ANO-FIM-PER-ULT   PIC 9(004).
              10 SLIG00-MES-FIM-PER-ULT   PIC 9(002).
              10 SLIG00-DIA-FIM-PER-ULT   PIC 9(002).
           05 SLIG00-DT-INI-PER-PRX       PIC 9(008).
           05 SLIG00-DT-INI-PER-PRX-R REDEFINES SLIG00-DT-INI-PER-PRX.
              10 SLIG00-ANO-INI-PER-PRX   PIC 9(004).
              10 SLIG00-MES-INI-PER-PRX   PIC 9(002).
              10 SLIG00-DIA-INI-PER-PRX   PIC 9(002).
           05 SLIG00-DT-FIM-PER-PRX       PIC 9(008).
           05 SLIG00-DT-FIM-PER-PRX-R REDEFINES SLIG00-DT-FIM-PER-PRX.
              10 SLIG00-ANO-FIM-PER-PRX   PIC 9(004).
              10 SLIG00-MES-FIM-PER-PRX   PIC 9(002).
              10 SLIG00-DIA-FIM-PER-PRX   PIC 9(002).
           05 SLIG00-FILLER               PIC X(025).
