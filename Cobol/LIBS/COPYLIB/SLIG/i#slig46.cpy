      ***===========================================================***
      *** NOME INC                                     LENGTH=00300 ***
      *** I#SLIG46                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - PAGAMENTOS E BAIXAS VALIDADO  ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-VNDOR-BAXA-PGTO-VALIDADO.
           05 SLIG46-CGC-CPF-COMP           PIC 9(009) COMP-3.
           05 SLIG46-FILIAL-COMP            PIC 9(004) COMP-3.
           05 SLIG46-CONTROLE-COMP          PIC 9(002) COMP-3.
           05 SLIG46-TIPO-PESSOA-COMP       PIC X(001).
           05 SLIG46-BCO-COMP               PIC 9(003) COMP-3.
           05 SLIG46-AGENCIA-COMP           PIC 9(005) COMP-3.
           05 SLIG46-CONTA-COMP             PIC 9(013) COMP-3.
           05 SLIG46-NOME-COMP              PIC X(040).
           05 SLIG46-CPRODT                 PIC 9(003) COMP-3.
           05 SLIG46-CFAML                  PIC 9(001) COMP-3.
           05 SLIG46-CCONTR                 PIC 9(009) COMP-3.
           05 SLIG46-CSEQ                   PIC 9(003) COMP-3.
           05 SLIG46-CPARCELA               PIC 9(003) COMP-3.
           05 SLIG46-NUM-PAGTO              PIC 9(009) COMP-3.
           05 SLIG46-NOSSO-NUMERO           PIC 9(011) COMP-3.
           05 SLIG46-CGC-CPF-FORN           PIC 9(009) COMP-3.
           05 SLIG46-FILIAL-FORN            PIC 9(004) COMP-3.
           05 SLIG46-CONTROLE-FORN          PIC 9(002) COMP-3.
           05 SLIG46-TIPO-PESSOA-FORN       PIC X(001).
           05 SLIG46-BCO-FORN               PIC 9(003) COMP-3.
           05 SLIG46-AGENCIA-FORN           PIC 9(005) COMP-3.
           05 SLIG46-CONTA-FORN             PIC 9(013) COMP-3.
           05 SLIG46-NOME-FORN              PIC X(040).
           05 SLIG46-DT-VENCTO-PARC         PIC 9(008).
           05 SLIG46-DT-VENCTO-PARC-RED REDEFINES SLIG46-DT-VENCTO-PARC.
              10 SLIG46-ANO-VENCTO-PARC     PIC 9(004).
              10 SLIG46-MES-VENCTO-PARC     PIC 9(002).
              10 SLIG46-DIA-VENCTO-PARC     PIC 9(002).
           05 SLIG46-DT-PAGTO-PARC          PIC 9(008).
           05 SLIG46-DT-PAGTO-PARC-RED REDEFINES SLIG46-DT-PAGTO-PARC.
              10 SLIG46-ANO-PAGTO-PARC      PIC 9(004).
              10 SLIG46-MES-PAGTO-PARC      PIC 9(002).
              10 SLIG46-DIA-PAGTO-PARC      PIC 9(002).
           05 SLIG46-CTPO-SIT-PARC          PIC 9(003) COMP-3.
              88 SLIG46-CTPO-PAGTO-NORMAL   VALUE 1.
              88 SLIG46-CTPO-PAGTO-ANTECI   VALUE 2.
              88 SLIG46-CTPO-BAIXA-NORMAL   VALUE 3.
              88 SLIG46-CTPO-BAIXA-ANTECI   VALUE 4.
              88 SLIG46-CTPO-BAIXA-EXCLUS   VALUE 5.
           05 SLIG46-DT-ENTRADA-ORIGEM      PIC 9(008).
           05 SLIG46-DT-ENTRADA-ORIGEM-RED REDEFINES
              SLIG46-DT-ENTRADA-ORIGEM.
              10 SLIG46-ANO-ENTRADA-ORIGEM  PIC 9(004).
              10 SLIG46-MES-ENTRADA-ORIGEM  PIC 9(002).
              10 SLIG46-DIA-ENTRADA-ORIGEM  PIC 9(002).
           05 SLIG46-VL-PRINC-PARC          PIC 9(013)V99 COMP-3.
           05 SLIG46-IND-PAGTO              PIC X(001).
              88 SLIG46-IND-PAGTO-COMP      VALUE '1'.
              88 SLIG46-IND-PAGTO-FORN      VALUE '2'.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG46-CGC-CPF-COMP-ST        PIC X(009).
           05 SLIG46-FILIAL-COMP-ST         PIC X(004).
           05 SLIG46-CONTROLE-COMP-ST       PIC 9(002) COMP-3.
           05 SLIG46-CGC-CPF-FORN-ST        PIC X(009).
           05 SLIG46-FILIAL-FORN-ST         PIC X(004).
           05 SLIG46-CONTROLE-FORN-ST       PIC 9(002) COMP-3.
           05 SLIG46-FILLER                 PIC X(086).
