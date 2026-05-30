      ***===========================================================***
      *** NOME INC                                     LENGTH=00200 ***
      *** I#SLIG29                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - PAGAMENTOS E BAIXAS           ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-VENDOR-BAIXA-PAGAMENTO       PIC X(230).
       01  REG-TIPO-HEADER REDEFINES REG-VENDOR-BAIXA-PAGAMENTO.
           05 SLIG29-TIPO-REG-HEADER        PIC X(001).
              88 SLIG29-TIPO-HEADER         VALUE '1'.
           05 SLIG29-DT-MOVIMENTO           PIC 9(008).
           05 SLIG29-DT-MOVIMENTO-RED REDEFINES SLIG29-DT-MOVIMENTO.
              10 SLIG29-ANO-MOVIMENTO       PIC 9(004).
              10 SLIG29-MES-MOVIMENTO       PIC 9(002).
              10 SLIG29-DIA-MOVIMENTO       PIC 9(002).
           05 SLIG29-REG-SEQ-HEADER         PIC 9(005) COMP-3.
           05 SLIG29-FILLER1                PIC X(218).
       01  REG-TIPO-REGISTRO REDEFINES REG-VENDOR-BAIXA-PAGAMENTO.
           05 SLIG29-TIPO-REG               PIC X(001).
              88 SLIG29-TIPO-REGISTRO       VALUE '2'.
           05 SLIG29-CGC-CPF-COMP           PIC 9(009) COMP-3.
           05 SLIG29-FILIAL-COMP            PIC 9(004) COMP-3.
           05 SLIG29-CONTROLE-COMP          PIC 9(002) COMP-3.
           05 SLIG29-TIPO-PESSOA-COMP       PIC X(001).
           05 SLIG29-BCO-COMP               PIC 9(003) COMP-3.
           05 SLIG29-AGENCIA-COMP           PIC 9(005) COMP-3.
           05 SLIG29-CONTA-COMP             PIC 9(013) COMP-3.
           05 SLIG29-NOME-COMP              PIC X(040).
           05 SLIG29-CPRODT                 PIC 9(003) COMP-3.
           05 SLIG29-CFAML                  PIC 9(001) COMP-3.
           05 SLIG29-CCONTR                 PIC 9(009) COMP-3.
           05 SLIG29-CSEQ                   PIC 9(003) COMP-3.
           05 SLIG29-CPARCELA               PIC 9(003) COMP-3.
           05 SLIG29-NUM-PAGTO              PIC 9(009) COMP-3.
           05 SLIG29-NOSSO-NUMERO           PIC 9(011) COMP-3.
           05 SLIG29-CGC-CPF-FORN           PIC 9(009) COMP-3.
           05 SLIG29-FILIAL-FORN            PIC 9(004) COMP-3.
           05 SLIG29-CONTROLE-FORN          PIC 9(002) COMP-3.
           05 SLIG29-TIPO-PESSOA-FORN       PIC X(001).
           05 SLIG29-BCO-FORN               PIC 9(003) COMP-3.
           05 SLIG29-AGENCIA-FORN           PIC 9(005) COMP-3.
           05 SLIG29-CONTA-FORN             PIC 9(013) COMP-3.
           05 SLIG29-NOME-FORN              PIC X(040).
           05 SLIG29-DT-VENCTO-PARC         PIC 9(008).
           05 SLIG29-DT-VENCTO-PARC-RED REDEFINES SLIG29-DT-VENCTO-PARC.
              10 SLIG29-ANO-VENCTO-PARC     PIC 9(004).
              10 SLIG29-MES-VENCTO-PARC     PIC 9(002).
              10 SLIG29-DIA-VENCTO-PARC     PIC 9(002).
           05 SLIG29-DT-PAGTO-PARC          PIC 9(008).
           05 SLIG29-DT-PAGTO-PARC-RED REDEFINES SLIG29-DT-PAGTO-PARC.
              10 SLIG29-ANO-PAGTO-PARC      PIC 9(004).
              10 SLIG29-MES-PAGTO-PARC      PIC 9(002).
              10 SLIG29-DIA-PAGTO-PARC      PIC 9(002).
           05 SLIG29-CTPO-SIT-PARC          PIC 9(003) COMP-3.
              88 SLIG29-CTPO-PAGTO-NORMAL   VALUE 1.
              88 SLIG29-CTPO-PAGTO-ANTECI   VALUE 2.
              88 SLIG29-CTPO-BAIXA-NORMAL   VALUE 3.
              88 SLIG29-CTPO-BAIXA-ANTECI   VALUE 4.
              88 SLIG29-CTPO-BAIXA-EXCLUS   VALUE 5.
           05 SLIG29-DT-ENTRADA-ORIGEM      PIC 9(008).
           05 SLIG29-DT-ENTRADA-ORIGEM-RED REDEFINES
              SLIG29-DT-ENTRADA-ORIGEM.
              10 SLIG29-ANO-ENTRADA-ORIGEM  PIC 9(004).
              10 SLIG29-MES-ENTRADA-ORIGEM  PIC 9(002).
              10 SLIG29-DIA-ENTRADA-ORIGEM  PIC 9(002).
           05 SLIG29-VL-PRINC-PARC          PIC 9(013)V99 COMP-3.
           05 SLIG29-IND-PAGTO              PIC X(001).
              88 SLIG29-IND-PAGTO-COMP      VALUE '1'.
              88 SLIG29-IND-PAGTO-FORN      VALUE '2'.
           05 SLIG29-REG-SEQ                PIC 9(005) COMP-3.
           05 SLIG29-AGENCIA-NEGOC          PIC 9(005) COMP-3.
           05 SLIG29-IND-ULTIMA-PARCELA     PIC X(001).
      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG29-CGC-CPF-COMP-ST        PIC X(009).
           05 SLIG29-FILIAL-COMP-ST         PIC X(004).
           05 SLIG29-CONTROLE-COMP-ST       PIC 9(002) COMP-3.
           05 SLIG29-CGC-CPF-FORN-ST        PIC X(009).
           05 SLIG29-FILIAL-FORN-ST         PIC X(004).
           05 SLIG29-CONTROLE-FORN-ST       PIC 9(002) COMP-3.
           05 SLIG29-FILLER2                PIC X(008).
       01  REG-TIPO-TRAILER REDEFINES REG-VENDOR-BAIXA-PAGAMENTO.
           05 SLIG29-TIPO-REG-TRAILER       PIC X(001).
              88 SLIG29-TIPO-TRAILER        VALUE '3'.
           05 SLIG29-REG-SEQ-TRAILER        PIC 9(005) COMP-3.
           05 SLIG29-FILLER3                PIC X(226).
