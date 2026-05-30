      ***===========================================================***
      *** NOME INC                                     LENGTH=00300 ***
      *** I#SLIG31                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - COMPRADOR                     ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-VENDOR-COMPRADOR.
           05 SLIG31-CGC-CPF-COMP           PIC 9(009) COMP-3.
           05 SLIG31-FILIAL-COMP            PIC 9(004) COMP-3.
           05 SLIG31-CONTROLE-COMP          PIC 9(002) COMP-3.
           05 SLIG31-TIPO-PESSOA-COMP       PIC X(001).
           05 SLIG31-BCO-COMP               PIC 9(003) COMP-3.
           05 SLIG31-AGENCIA-COMP           PIC 9(005) COMP-3.
           05 SLIG31-CONTA-COMP             PIC 9(013) COMP-3.
           05 SLIG31-NOME-COMP              PIC X(040).
           05 SLIG31-CPRODT                 PIC 9(003) COMP-3.
           05 SLIG31-CFAML                  PIC 9(001) COMP-3.
           05 SLIG31-CCONTR                 PIC 9(009) COMP-3.
           05 SLIG31-CSEQ                   PIC 9(003) COMP-3.
           05 SLIG31-CPARCELA               PIC 9(003) COMP-3.
           05 SLIG31-NUM-PAGTO              PIC 9(009) COMP-3.
           05 SLIG31-NOSSO-NUMERO           PIC 9(011) COMP-3.
           05 SLIG31-CGC-CPF-FORN           PIC 9(009) COMP-3.
           05 SLIG31-FILIAL-FORN            PIC 9(004) COMP-3.
           05 SLIG31-CONTROLE-FORN          PIC 9(002) COMP-3.
           05 SLIG31-TIPO-PESSOA-FORN       PIC X(001).
           05 SLIG31-BCO-FORN               PIC 9(003) COMP-3.
           05 SLIG31-AGENCIA-FORN           PIC 9(005) COMP-3.
           05 SLIG31-CONTA-FORN             PIC 9(013) COMP-3.
           05 SLIG31-NOME-FORN              PIC X(040).
           05 SLIG31-DT-VENCTO-PARC         PIC 9(008).
           05 SLIG31-DT-VENCTO-PARC-RED REDEFINES SLIG31-DT-VENCTO-PARC.
              10 SLIG31-ANO-VENCTO-PARC     PIC 9(004).
              10 SLIG31-MES-VENCTO-PARC     PIC 9(002).
              10 SLIG31-DIA-VENCTO-PARC     PIC 9(002).
           05 SLIG31-DT-PAGTO-PARC          PIC 9(008).
           05 SLIG31-DT-PAGTO-PARC-RED REDEFINES SLIG31-DT-PAGTO-PARC.
              10 SLIG31-ANO-PAGTO-PARC      PIC 9(004).
              10 SLIG31-MES-PAGTO-PARC      PIC 9(002).
              10 SLIG31-DIA-PAGTO-PARC      PIC 9(002).
           05 SLIG31-CPTO-SIT-PARC          PIC 9(003) COMP-3.
              88 SLIG31-CTPO-PAGTO-NORMAL   VALUE 1.
              88 SLIG31-CTPO-PAGTO-ANTECI   VALUE 2.
              88 SLIG31-CTPO-BAIXA-NORMAL   VALUE 3.
              88 SLIG31-CTPO-BAIXA-ANTECI   VALUE 4.
              88 SLIG31-CTPO-BAIXA-EXCLUS   VALUE 5.
              88 SLIG31-CTPO-SIT-PARC-VENC  VALUE 6.
              88 SLIG31-CTPO-SIT-PARC-FUTU  VALUE 7.
           05 SLIG31-DT-ENTRADA-ORIGEM      PIC 9(008).
           05 SLIG31-DT-ENTRADA-ORIGEM-RED REDEFINES
              SLIG31-DT-ENTRADA-ORIGEM.
              10 SLIG31-ANO-ENTRADA-ORIGEM  PIC 9(004).
              10 SLIG31-MES-ENTRADA-ORIGEM  PIC 9(002).
              10 SLIG31-DIA-ENTRADA-ORIGEM  PIC 9(002).
           05 SLIG31-VL-PRINC-PARC          PIC 9(013)V99 COMP-3.
           05 SLIG31-IND-PAGTO              PIC X(001).
              88 SLIG31-IND-PAGTO-COMP      VALUE '1'.
              88 SLIG31-IND-PAGTO-FORN      VALUE '2'.
           05 SLIG31-BCO-DIR-REGIONAL       PIC 9(003) COMP-3.
           05 SLIG31-DIR-REGIONAL           PIC 9(005) COMP-3.
           05 SLIG31-BCO-GERC-REGIONAL      PIC 9(003) COMP-3.
           05 SLIG31-GERC-REGIONAL          PIC 9(005) COMP-3.
           05 SLIG31-GRP-ECONOMICO-COMP     PIC 9(009) COMP-3.
           05 SLIG31-GRP-ECONOMICO-FORN     PIC 9(009) COMP-3.
      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG31-CGC-CPF-COMP-ST        PIC X(009).
           05 SLIG31-FILIAL-COMP-ST         PIC X(004).
           05 SLIG31-CONTROLE-COMP-ST       PIC 9(002) COMP-3.
           05 SLIG31-CGC-CPF-FORN-ST        PIC X(009).
           05 SLIG31-FILIAL-FORN-ST         PIC X(004).
           05 SLIG31-CONTROLE-FORN-ST       PIC 9(002) COMP-3.
           05 SLIG31-FILLER                 PIC X(066).
