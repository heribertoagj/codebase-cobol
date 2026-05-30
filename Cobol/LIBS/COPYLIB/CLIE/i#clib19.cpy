      *---------------------------------------------------------------*
      *     AREA DE SAIDA - MODULO MULTIBANCO - CHAVE UNICA           *
      *                                                               *
      *     MODULO CLIB1003 - DADOS DE PRODUTO (CONTA CORRENTE E      *
      *                                         POUPANCA)             *
      *                                                               *
      *     INC = I#CLIB19                                            *
      *---------------------------------------------------------------*


       01  WRK-CLIBB007.
           03 WRK-CBCO              PIC S9(3)V COMP-3   VALUE ZEROS.
           03 WRK-CAG-BCRIA         PIC S9(5)V COMP-3   VALUE ZEROS.
           03 WRK-CCTA-BCRIA-CLI    PIC S9(13)V COMP-3  VALUE ZEROS.
           03 WRK-CGRP-CTBIL-ATUAL  PIC S9(3)V COMP-3   VALUE ZEROS.
           03 WRK-CSGRP-CTBIL       PIC S9(3)V COMP-3   VALUE ZEROS.
           03 WRK-CTPO-CTA          PIC S9(2)V COMP-3   VALUE ZEROS.
           03 WRK-CFORMA-MOVTC-CTA  PIC S9(1)V COMP-3   VALUE ZEROS.
           03 WRK-CPERDC-EMIS-EXTRT PIC X               VALUE SPACES.
           03 WRK-DABERT-CTA        PIC X(10)           VALUE SPACES.
           03 WRK-DULT-ACERT        PIC X(10)           VALUE SPACES.
           03 WRK-CSIT-CTA-MOVTC    PIC X               VALUE SPACES.
           03 WRK-DRCADT            PIC X(10)           VALUE SPACES.
           03 WRK-ICTA-MOVTC        PIC X(40)           VALUE SPACES.
           03 WRK-ELOGDR            PIC X(40)           VALUE SPACES.
           03 WRK-ENRO-IMOV         PIC X(7)            VALUE SPACES.
           03 WRK-ECOMPL-NRO-LOGDR  PIC X(20)           VALUE SPACES.
           03 WRK-EBAIRO-LOGDR      PIC X(20)           VALUE SPACES.
           03 WRK-CPOSTO-SERVC      PIC S9(3)V COMP-3   VALUE ZEROS.
           03 WRK-CID-CLI           PIC X(26)           VALUE SPACES.
           03 WRK-CCEP              PIC S9(5)V COMP-3   VALUE ZEROS.
           03 WRK-CCEP-COMPL        PIC S9(3)V COMP-3   VALUE ZEROS.
           03 WRK-CUSUAR-SENHA      PIC X(7)            VALUE SPACES.
           03 WRK-ELOGDR-NRO-COMPL  PIC X(40)           VALUE SPACES.
           03 WRK-CCEP-NOVO         PIC S9(5)V COMP-3   VALUE ZEROS.
           03 WRK-CCEP-NOVO-COMPL   PIC S9(3)V COMP-3   VALUE ZEROS.
           03 WRK-CORIGE-CTA-ABERT  PIC S9(2)V COMP-3   VALUE ZEROS.
           03 FILLER                PIC X(733)          VALUE SPACES.

