      ***===========================================================***
      *** NOME INC                                     LENGTH=00054 ***
      *** I#SLIG8W                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            IMAGEM DA TABELA DB2PRD.TLIQDZ_EMITT_CTA       ***
      ***                                                           ***
      ***===========================================================***
      *
       01  SLIG8W-REGISTRO.
           05 SLIG8W-CBCO                        PIC S9(03)    COMP-3.
           05 SLIG8W-CAG-BCRIA                   PIC S9(05)    COMP-3.
           05 SLIG8W-CCTA-BCRIA-CLI              PIC S9(13)    COMP-3.
           05 SLIG8W-CSEQ-CLASF-SACDO            PIC S9(15)    COMP-3.
           05 SLIG8W-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
           05 SLIG8W-CFLIAL-CGC-SACDO            PIC S9(005)   COMP-3.
           05 SLIG8W-CCTRL-CGC-SACDO             PIC S9(002)   COMP-3.
           05 SLIG8W-CINDCD-PSSOA-SACDO          PIC X(001).
           05 SLIG8W-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
           05 SLIG8W-CFLIAL-CGC-CLI              PIC S9(05)    COMP-3.
           05 SLIG8W-CCTRL-CGC-CLI               PIC S9(02)    COMP-3.
           05 SLIG8W-CINDCD-PSSOA-CLI            PIC X(001).
           05 SLIG8W-IL-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG8W-IL-SIMPLES-NULL             PIC  X(01).
           05 SLIG8W-IL-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG8W-IL-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG8W-IL-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG8W-IL-DESCONTO-NULL            PIC  X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG8W-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG8W-CFLIAL-CGC-SACDO-ST         PIC X(005).
           05 SLIG8W-CCTRL-CGC-SACDO-ST          PIC S9(002)   COMP-3.
           05 SLIG8W-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8W-CFLIAL-CGC-CLI-ST           PIC X(005).
           05 SLIG8W-CCTRL-CGC-CLI-ST            PIC S9(02)    COMP-3.
