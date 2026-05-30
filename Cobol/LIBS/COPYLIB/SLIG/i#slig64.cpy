      ***===========================================================***
      *** NOME INC                                     LENGTH=00300 ***
      *** I#SLIG64                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - FINANCEIRO ATIVO VALIDADO     ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-VNDOR-FINCR-ATIVO-VALIDADO.
           05 SLIG64-CPRODT                 PIC 9(003) COMP-3.
           05 SLIG64-CFAML                  PIC 9(001) COMP-3.
           05 SLIG64-CCONTR                 PIC 9(009) COMP-3.
           05 SLIG64-CSEQ                   PIC 9(003) COMP-3.
           05 SLIG64-CPARCELA               PIC 9(003) COMP-3.
           05 SLIG64-NOSSO-NUMERO           PIC 9(011) COMP-3.
           05 SLIG64-CGC-CPF-COMP           PIC 9(009) COMP-3.
           05 SLIG64-FILIAL-COMP            PIC 9(004) COMP-3.
           05 SLIG64-CONTROLE-COMP          PIC 9(002) COMP-3.
           05 SLIG64-TIPO-PESSOA-COMP       PIC X(001).
           05 SLIG64-BCO-COMP               PIC 9(003) COMP-3.
           05 SLIG64-AGENCIA-COMP           PIC 9(005) COMP-3.
           05 SLIG64-CONTA-COMP             PIC 9(013) COMP-3.
           05 SLIG64-NOME-COMP              PIC X(040).
           05 SLIG64-CGC-CPF-FORN           PIC 9(009) COMP-3.
           05 SLIG64-FILIAL-FORN            PIC 9(004) COMP-3.
           05 SLIG64-CONTROLE-FORN          PIC 9(002) COMP-3.
           05 SLIG64-TIPO-PESSOA-FORN       PIC X(001).
           05 SLIG64-BCO-FORN               PIC 9(003) COMP-3.
           05 SLIG64-AGENCIA-FORN           PIC 9(005) COMP-3.
           05 SLIG64-CONTA-FORN             PIC 9(013) COMP-3.
           05 SLIG64-NOME-FORN              PIC X(040).
           05 SLIG64-VL-PRINC-PARC          PIC 9(013)V99 COMP-3.
           05 SLIG64-DT-VENCTO-PARC         PIC 9(008).
           05 SLIG64-DT-VENCTO-PARC-RED REDEFINES SLIG64-DT-VENCTO-PARC.
              10 SLIG64-ANO-VENCTO-PARC     PIC 9(004).
              10 SLIG64-MES-VENCTO-PARC     PIC 9(002).
              10 SLIG64-DIA-VENCTO-PARC     PIC 9(002).
           05 SLIG64-DT-ENTRADA-ORIGEM      PIC 9(008).
           05 SLIG64-DT-ENTRADA-ORIGEM-RED REDEFINES
              SLIG64-DT-ENTRADA-ORIGEM.
              10 SLIG64-ANO-ENTRADA-ORIGEM  PIC 9(004).
              10 SLIG64-MES-ENTRADA-ORIGEM  PIC 9(002).
              10 SLIG64-DIA-ENTRADA-ORIGEM  PIC 9(002).
           05 SLIG64-CTPO-SIT-PARC          PIC 9(003) COMP-3.
              88 SLIG64-CTPO-SIT-PARC-VENC  VALUE 1.
              88 SLIG64-CTPO-SIT-PARC-FUTU  VALUE 2.
           05 SLIG64-CGC-CPF-COMP-ST        PIC X(009).
           05 SLIG64-FILIAL-COMP-ST         PIC X(004).
           05 SLIG64-CONTROLE-COMP-ST       PIC 9(002) COMP-3.
           05 SLIG64-CGC-CPF-FORN-ST        PIC X(009).
           05 SLIG64-FILIAL-FORN-ST         PIC X(004).
           05 SLIG64-CONTROLE-FORN-ST       PIC 9(002) COMP-3.
           05 SLIG64-FILLER                 PIC X(100).
