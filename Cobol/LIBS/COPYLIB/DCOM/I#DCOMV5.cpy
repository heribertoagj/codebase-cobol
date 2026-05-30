      ***===========================================================***
      *** NOME INC                                     LENGTH=00700 ***
      *** I#DCOMV5                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: REGISTRO DO ARQUIVO DE TITULOS ALTERADOS       ***
      *** COPIA DA I#CBON73                                         ***
      ***                                                           ***
      ***===========================================================***

       01  DCOMV5-REG.
         03  DCOMV5-TIT-PENDENTE-ALTERADO.
           05  DCOMV5-PRODUTO               PIC 9(02)    COMP-3.
           05  DCOMV5-CNEGOC                PIC 9(11)    COMP-3.
           05  DCOMV5-NOSNRO                PIC 9(11)    COMP-3.
           05  DCOMV5-SEQUENCIA             PIC 9(03)    COMP-3.
           05  DCOMV5-CD-AUTO-AT            PIC 9(15)    COMP-3.
           05  DCOMV5-CLI-ESP               PIC 9(07)    COMP-3.
           05  DCOMV5-CD-BAN-DEP            PIC 9(03)    COMP-3.
           05  DCOMV5-CD-AGE-DEP            PIC 9(05)    COMP-3.
           05  DCOMV5-STATUS                PIC 9(03)    COMP-3.
           05  DCOMV5-IND-SAC               PIC 9(01)    COMP-3.
           05  DCOMV5-IND-DBA               PIC 9(01)    COMP-3.
           05  DCOMV5-IND-PROT              PIC 9(01)    COMP-3.
           05  DCOMV5-IND-REAT              PIC 9(01)    COMP-3.
           05  DCOMV5-IND-REAP              PIC 9(01)    COMP-3.
           05  DCOMV5-IND-PGTO              PIC 9(01)    COMP-3.
           05  DCOMV5-IND-RAT               PIC X(01).
           05  DCOMV5-IND-DTVEN             PIC 9(01)    COMP-3.
           05  DCOMV5-IND-MANUT             PIC 9(03)    COMP-3.
           05  DCOMV5-ARQ-SEQ-ORIGEM        PIC 9(02).
           05  DCOMV5-DATVEN                PIC 9(09)    COMP-3.
           05  DCOMV5-DATEMI                PIC 9(09)    COMP-3.
           05  DCOMV5-DATREA                PIC 9(09)    COMP-3.
           05  DCOMV5-DATREG                PIC 9(09)    COMP-3.
           05  DCOMV5-COD-NTAX              PIC 9(05)    COMP-3.
           05  DCOMV5-VALOR                 PIC 9(17)    COMP-3.
           05  DCOMV5-ABATIM                PIC 9(17)    COMP-3.
           05  DCOMV5-IOF                   PIC 9(17)    COMP-3.
           05  DCOMV5-SEUNRO                PIC X(15).
           05  DCOMV5-CPARTI                PIC X(25).
           05  DCOMV5-SACADO                PIC X(40).
           05  DCOMV5-ENDERECO              PIC X(40).
           05  DCOMV5-CEP                   PIC 9(05)    COMP-3.
           05  DCOMV5-CEP-SUFIXO            PIC X(03).
           05  DCOMV5-TIPINS                PIC 9(01)    COMP-3.
           05  DCOMV5-INSCRICAO             PIC 9(15)    COMP-3.
           05  DCOMV5-ST-CGC-CPF            PIC 9(01)    COMP-3.
           05  DCOMV5-DESC-COD1             PIC X(01).
           05  DCOMV5-DESC-DAT1             PIC 9(09)    COMP-3.
           05  DCOMV5-DESC-VAL1             PIC 9(15)    COMP-3.
           05  DCOMV5-DESC-COD2             PIC X(01).
           05  DCOMV5-DESC-DAT2             PIC 9(09)    COMP-3.
           05  DCOMV5-DESC-VAL2             PIC 9(15)    COMP-3.
           05  DCOMV5-DESC-COD3             PIC X(01).
           05  DCOMV5-DESC-DAT3             PIC 9(09)    COMP-3.
           05  DCOMV5-DESC-VAL3             PIC 9(15)    COMP-3.
           05  DCOMV5-JUROS-COD             PIC X(01).
           05  DCOMV5-JUROS-DAT             PIC 9(09)    COMP-3.
           05  DCOMV5-JUROS-VAL             PIC 9(17)    COMP-3.
           05  DCOMV5-MULTA-COD             PIC X(01).
           05  DCOMV5-MULTA-DAT             PIC 9(09)    COMP-3.
           05  DCOMV5-MULTA-VAL             PIC 9(17)    COMP-3.
           05  DCOMV5-PROT-AUT              PIC X(01).
           05  DCOMV5-NUMDIAS               PIC 9(03)    COMP-3.
           05  DCOMV5-BX-DEV-COD            PIC X(01).
           05  DCOMV5-BX-DEV-DIA            PIC 9(03)    COMP-3.
           05  DCOMV5-COBCAR                PIC X(01).
           05  DCOMV5-TIPDOC                PIC X(01).
           05  DCOMV5-EMIBLOQ               PIC X(01).
           05  DCOMV5-DISBLOQ               PIC X(01).
           05  DCOMV5-ESPECIE               PIC 9(03)    COMP-3.
           05  DCOMV5-ACEITE                PIC X(01).
           05  DCOMV5-SUB-ORIGEM            PIC 9(05)    COMP-3.
           05  DCOMV5-TRT-PPLETA            PIC 9(05)    COMP-3.
           05  DCOMV5-COD-ORIGEM            PIC 9(03)    COMP-3.
           05  DCOMV5-AGE-OPER              PIC 9(05)    COMP-3.
           05  DCOMV5-NUM-EMPR              PIC 9(07)    COMP-3.
           05  DCOMV5-CIP                   PIC 9(03)    COMP-3.
           05  DCOMV5-SAC-AVAL              PIC X(40).
           05  DCOMV5-TIPINS-AVAL           PIC 9(01)    COMP-3.
           05  DCOMV5-INSC-AVAL             PIC 9(15)    COMP-3.
           05  DCOMV5-ST-SAC-AVAL           PIC 9(01)    COMP-3.
           05  DCOMV5-BCO-DEB-AUT           PIC 9(03)    COMP-3.
           05  DCOMV5-AGE-DEB-AUT           PIC 9(05)    COMP-3.
           05  DCOMV5-DIG-AGE-DA            PIC X(01).
           05  DCOMV5-GRP-DEB-AUT           PIC 9(03)    COMP-3.
           05  DCOMV5-SGRP-DEB-AUT          PIC 9(03)    COMP-3.
           05  DCOMV5-CCTA-DEB-AUT          PIC 9(13)    COMP-3.
           05  DCOMV5-DCTA-DEB-AUT          PIC X(01).
           05  DCOMV5-DCTAE-DEB-AUT         PIC X(01).
           05  DCOMV5-ENDER-AVISO           PIC X(01).
           05  DCOMV5-IND-EMI-AVIS          PIC X(01).
           05  DCOMV5-TIP-FOR-RET           PIC 9(03)    COMP-3.
           05  DCOMV5-TIP-MOT-RET           PIC 9(03)    COMP-3.
           05  DCOMV5-MOT-RET-SIS           PIC 9(03)    COMP-3.
           05  DCOMV5-DAT-DESAGENDA         PIC 9(09)    COMP-3.
           05  DCOMV5-PROT-DAT-INST         PIC 9(09)    COMP-3.
           05  DCOMV5-DAT-ENV-CART          PIC 9(09)    COMP-3.
           05  DCOMV5-DAT-SOL-SUST          PIC 9(09)    COMP-3.
           05  DCOMV5-DAT-EFE-SUST          PIC 9(09)    COMP-3.
           05  DCOMV5-DAT-CONS-SUST         PIC 9(09)    COMP-3.
           05  DCOMV5-IND-CON-SUST          PIC 9(01)    COMP-3.
           05  DCOMV5-NUM-CARTORIO          PIC X(05).
           05  DCOMV5-NUM-PROTOCOL          PIC X(15).
           05  DCOMV5-DEV-SUST              PIC 9(03)    COMP-3.
           05  DCOMV5-BAN-PROT              PIC 9(03)    COMP-3.
           05  DCOMV5-AGE-PROT              PIC 9(05)    COMP-3.
           05  DCOMV5-TIPO-ENDOSSO          PIC X(01).
           05  DCOMV5-AREA-RESERVA.
             10  DCOMV5-TIT-LIVRE             PIC X(39).
             10  DCOMV5-RES1                  PIC X(140).
           05  DCOMV5-AREA-ETAPA3  REDEFINES  DCOMV5-AREA-RESERVA.
             10  DCOMV5-SEQUENCIA-PRACA       PIC 9(03)    COMP-3.
             10  DCOMV5-IDENT-COB-PROTE       PIC X(01).
             10  DCOMV5-DATA-FIM-PROTE        PIC 9(09)    COMP-3.
             10  DCOMV5-DATA-ENCAR-PROTE      PIC 9(09)    COMP-3.
             10  DCOMV5-VALOR-ENCAR-PROTE     PIC 9(15)V9(02) COMP-3.
             10  DCOMV5-QDIA-ENCAR-PROTE      PIC 9(04)    COMP-3.
             10  DCOMV5-CTPO-COBR-ENCAR       PIC X(01).
             10  DCOMV5-IND-TITLO-SOLUC       PIC X(01).
             10  DCOMV5-IND-PAGTO-INDEVIDO    PIC X(01).
             10  DCOMV5-IND-SIT-IMPRE         PIC X(01).
             10  DCOMV5-DATA-CARTA-SUST       PIC 9(09)    COMP-3.
             10  DCOMV5-HORA-SOLIC-SUST       PIC 9(11)    COMP-3.
             10  DCOMV5-ORIGEM-INST-PROTE     PIC 9(02)    COMP-3.
             10  DCOMV5-IND-CONSULTA-SUS      PIC X(01).
             10  DCOMV5-VERSAO-REMESSA        PIC 9(06) COMP-3.
             10  DCOMV5-CEP-SAC-AVAL          PIC 9(05)    COMP-3.
             10  DCOMV5-CEP-COMPL-SAC-AVAL    PIC 9(03)    COMP-3.
             10  DCOMV5-ENDERECO-SAC-AVAL     PIC X(40).
             10  DCOMV5-BANCO-PRACA           PIC 9(03)    COMP-3.
             10  DCOMV5-AGENCIA-PRACA         PIC 9(05)    COMP-3.
             10  DCOMV5-DATA-VERSAO-REM       PIC 9(09)    COMP-3.
             10  DCOMV5-RESERVA-ETAPA3        PIC X(77).
         03  DCOMV5-COMPLEMENTO-BAIXADO.
           05  DCOMV5-ORIGEM-BAIXA          PIC S9(2)V   COMP-3.
           05  DCOMV5-DATA-BAIXA            PIC X(10).
           05  DCOMV5-SITUACAO-ANTERIOR     PIC S9(2)V   COMP-3.
           05  DCOMV5-OCORRENCIA-REMESSA    PIC S9(3)V   COMP-3.
           05  DCOMV5-MOTIVO-OCORRENCIA     PIC S9(3)V   COMP-3.
           05  DCOMV5-AGENCIA-PROCEDENTE    PIC S9(5)V   COMP-3.
           05  DCOMV5-RECEB-BCO             PIC 9(003)   COMP-3.
           05  DCOMV5-RECEB-AGEN            PIC 9(005)   COMP-3.
           05  DCOMV5-FILLER                PIC X(024).
