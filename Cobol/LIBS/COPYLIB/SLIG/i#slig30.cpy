      ***===========================================================***
      *** NOME INC                                     LENGTH=00200 ***
      *** I#SLIG30                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - FINANCEIRO ATIVOS             ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-VENDOR-FINANCEIRO-ATIVO      PIC X(200).
       01  REG-TIPO-HEADER REDEFINES REG-VENDOR-FINANCEIRO-ATIVO.
           05 SLIG30-TIPO-HEADER            PIC X(001).
              88 SLIG30-TIPO-REG1           VALUE '1'.
           05 SLIG30-DT-MOVIMENTO           PIC 9(008).
           05 SLIG30-DT-MOVIMENTO-RED REDEFINES SLIG30-DT-MOVIMENTO.
              10 SLIG30-ANO-MOVIMENTO       PIC 9(004).
              10 SLIG30-MES-MOVIMENTO       PIC 9(002).
              10 SLIG30-DIA-MOVIMENTO       PIC 9(002).
           05 SLIG30-REG-SEQ-HEADER         PIC 9(005) COMP-3.
           05 SLIG30-FILLER1                PIC X(188).
       01  REG-TIPO-CONTRATO REDEFINES REG-VENDOR-FINANCEIRO-ATIVO.
           05 SLIG30-TIPO-CONTRATO          PIC X(001).
              88 SLIG30-TIPO-REG2           VALUE '2'.
           05 SLIG30-CPRODT                 PIC 9(003) COMP-3.
           05 SLIG30-CFAML                  PIC 9(001) COMP-3.
           05 SLIG30-CCONTR                 PIC 9(009) COMP-3.
           05 SLIG30-CSEQ                   PIC 9(003) COMP-3.
           05 SLIG30-CGC-CPF-COMP           PIC 9(009) COMP-3.
           05 SLIG30-FILIAL-COMP            PIC 9(004) COMP-3.
           05 SLIG30-CONTROLE-COMP          PIC 9(002) COMP-3.
           05 SLIG30-TIPO-PESSOA-COMP       PIC X(001).
           05 SLIG30-BCO-COMP               PIC 9(003) COMP-3.
           05 SLIG30-AGENCIA-COMP           PIC 9(005) COMP-3.
           05 SLIG30-CONTA-COMP             PIC 9(013) COMP-3.
           05 SLIG30-NOME-COMP              PIC X(040).
           05 SLIG30-CGC-CPF-FORN           PIC 9(009) COMP-3.
           05 SLIG30-FILIAL-FORN            PIC 9(004) COMP-3.
           05 SLIG30-CONTROLE-FORN          PIC 9(002) COMP-3.
           05 SLIG30-TIPO-PESSOA-FORN       PIC X(001).
           05 SLIG30-BCO-FORN               PIC 9(003) COMP-3.
           05 SLIG30-AGENCIA-FORN           PIC 9(005) COMP-3.
           05 SLIG30-CONTA-FORN             PIC 9(013) COMP-3.
           05 SLIG30-NOME-FORN              PIC X(040).
           05 SLIG30-DT-ENTRADA-ORIGEM      PIC 9(008).
           05 SLIG30-DT-ENTRADA-ORIGEM-RED REDEFINES
              SLIG30-DT-ENTRADA-ORIGEM.
              10 SLIG30-ANO-ENTRADA-ORIGEM  PIC 9(004).
              10 SLIG30-MES-ENTRADA-ORIGEM  PIC 9(002).
              10 SLIG30-DIA-ENTRADA-ORIGEM  PIC 9(002).
           05 SLIG30-REG-SEQ2               PIC 9(005) COMP-3.
      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG30-CGC-CPF-COMP-ST        PIC X(009).
           05 SLIG30-FILIAL-COMP-ST         PIC X(004).
           05 SLIG30-CONTROLE-COMP-ST       PIC 9(002) COMP-3.
           05 SLIG30-CGC-CPF-FORN-ST        PIC X(009).
           05 SLIG30-FILIAL-FORN-ST         PIC X(004).
           05 SLIG30-CONTROLE-FORN-ST       PIC 9(002) COMP-3.
           05 SLIG30-FILLER2                PIC X(022).
       01  REG-TIPO-PARCELA REDEFINES REG-VENDOR-FINANCEIRO-ATIVO.
           05 SLIG30-TIPO-PARCELA           PIC X(001).
              88 SLIG30-TIPO-REG3           VALUE '3'.
           05 SLIG30-CPRODT                 PIC 9(003) COMP-3.
           05 SLIG30-CFAML                  PIC 9(001) COMP-3.
           05 SLIG30-CCONTR                 PIC 9(009) COMP-3.
           05 SLIG30-CSEQ                   PIC 9(003) COMP-3.
           05 SLIG30-CPARCELA               PIC 9(003) COMP-3.
           05 SLIG30-NOSSO-NUMERO           PIC 9(011) COMP-3.
           05 SLIG30-VL-PRINC-PARC          PIC 9(013)V99 COMP-3.
           05 SLIG30-DT-VENCTO-PARC         PIC 9(008).
           05 SLIG30-DT-VENCTO-PARC-RED REDEFINES SLIG30-DT-VENCTO-PARC.
              10 SLIG30-ANO-VENCTO-PARC     PIC 9(004).
              10 SLIG30-MES-VENCTO-PARC     PIC 9(002).
              10 SLIG30-DIA-VENCTO-PARC     PIC 9(002).
           05 SLIG30-CTPO-SIT-PARC          PIC 9(003) COMP-3.
              88 SLIG30-CTPO-SIT-PARC-VENC  VALUE 6.
              88 SLIG30-CTPO-SIT-PARC-FUTU  VALUE 7.
           05 SLIG30-REG-SEQ3               PIC 9(005) COMP-3.
           05 SLIG30-FILLER3                PIC X(160).
       01  REG-TIPO-TRAILER REDEFINES REG-VENDOR-FINANCEIRO-ATIVO.
           05 SLIG30-TIPO-TRAILER           PIC X(001).
              88 SLIG30-TIPO-REG4           VALUE '4'.
           05 SLIG30-REG-SEQ-TRAILER        PIC 9(005) COMP-3.
           05 SLIG30-FILLER4                PIC X(196).
