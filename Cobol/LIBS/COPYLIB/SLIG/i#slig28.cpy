      ***===========================================================***
      *** NOME INC                                     LENGTH=00300 ***
      *** I#SLIG28                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - EMITENTE                      ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-CHEQUE-EMITENTE.
           05 SLIG28-CGC-CPF-EMITT          PIC 9(009) COMP-3.
           05 SLIG28-FILIAL-EMITT           PIC 9(004) COMP-3.
           05 SLIG28-CONTROLE-EMITT         PIC 9(002) COMP-3.
           05 SLIG28-TIPO-PESSOA-EMITT      PIC X(001).
           05 SLIG28-BCO-EMITT              PIC 9(003) COMP-3.
           05 SLIG28-AGENCIA-EMITT          PIC 9(005) COMP-3.
           05 SLIG28-CONTA-EMITT            PIC 9(013) COMP-3.
           05 SLIG28-NUM-CHEQUE             PIC 9(007) COMP-3.
           05 SLIG28-DIG-CHEQUE             PIC X(001).
           05 SLIG28-NUM-LOTE               PIC 9(007) COMP-3.
           05 SLIG28-CARTEIRA               PIC 9(003) COMP-3.
              88 SLIG28-CARTEIRA-SIMPLES    VALUE 1.
              88 SLIG28-CARTEIRA-CAUCIONADA VALUE 2.
              88 SLIG28-CARTEIRA-DESCONTO   VALUE 3.
           05 SLIG28-NOME-EMITT             PIC X(040).
           05 SLIG28-BCO-CLI                PIC 9(003) COMP-3.
           05 SLIG28-AGENCIA-CLI            PIC 9(005) COMP-3.
           05 SLIG28-CONTA-CLI              PIC 9(013) COMP-3.
           05 SLIG28-CGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIG28-FILIAL-CLI             PIC 9(004) COMP-3.
           05 SLIG28-CONTROLE-CLI           PIC 9(002) COMP-3.
           05 SLIG28-TIPO-PESSOA-CLI        PIC X(001).
           05 SLIG28-NOME-CLI               PIC X(040).
           05 SLIG28-VL-PRINC-CHEQ          PIC 9(013)V99 COMP-3.
           05 SLIG28-BCO-DIR-REGIONAL       PIC 9(003) COMP-3.
           05 SLIG28-DIR-REGIONAL           PIC 9(005) COMP-3.
           05 SLIG28-BCO-GERC-REGIONAL      PIC 9(003) COMP-3.
           05 SLIG28-GERC-REGIONAL          PIC 9(005) COMP-3.
           05 SLIG28-GRP-ECONOMICO-EMITT    PIC 9(009) COMP-3.
           05 SLIG28-GRP-ECONOMICO-CLI      PIC 9(009) COMP-3.
           05 SLIG28-ULT-CTPO-CHEQ          PIC 9(003) COMP-3.
      *
      *       EXCLUIDO                       = 001
      *       COMPENSADO                     = 002
      *       REAPRESENTADO                  = 003
      *       NORMAL                         = 004
      *       DEVOLVIDO                      = 005
           05 SLIG28-VENCTO-ANT.
              10 SLIG28-TOT-ANT-ABER        PIC 9(03) COMP-3.
              10 SLIG28-TOT-ANT-COMP        PIC 9(03) COMP-3.
              10 SLIG28-TOT-ANT-REAP-S-RESP PIC 9(03) COMP-3.
              10 SLIG28-TOT-ANT-REAP-N-RESP PIC 9(03) COMP-3.
              10 SLIG28-TOT-ANT-DEVL-S-RESP PIC 9(03) COMP-3.
              10 SLIG28-TOT-ANT-DEVL-S-REAP PIC 9(03) COMP-3.
              10 SLIG28-TOT-ANT-DEVL-N-REAP PIC 9(03) COMP-3.
              10 SLIG28-TOT-ANT-BAIX        PIC 9(03) COMP-3.
           05 SLIG28-VENCTO-PER.
              10 SLIG28-TOT-PER-ABER        PIC 9(03) COMP-3.
              10 SLIG28-TOT-PER-COMP        PIC 9(03) COMP-3.
              10 SLIG28-TOT-PER-REAP-S-RESP PIC 9(03) COMP-3.
              10 SLIG28-TOT-PER-REAP-N-RESP PIC 9(03) COMP-3.
              10 SLIG28-TOT-PER-DEVL-S-RESP PIC 9(03) COMP-3.
              10 SLIG28-TOT-PER-DEVL-S-REAP PIC 9(03) COMP-3.
              10 SLIG28-TOT-PER-DEVL-N-REAP PIC 9(03) COMP-3.
              10 SLIG28-TOT-PER-BAIX        PIC 9(03) COMP-3.
              10 SLIG28-TOT-PER-BAX-ANT     PIC 9(03) COMP-3.
           05 SLIG28-VENCTO-FUT.
              10 SLIG28-TOT-FUT-ABER        PIC 9(03) COMP-3.
              10 SLIG28-TOT-FUT-COMP        PIC 9(03) COMP-3.
              10 SLIG28-TOT-FUT-REAP-S-RESP PIC 9(03) COMP-3.
              10 SLIG28-TOT-FUT-REAP-N-RESP PIC 9(03) COMP-3.
              10 SLIG28-TOT-FUT-DEVL-S-RESP PIC 9(03) COMP-3.
              10 SLIG28-TOT-FUT-DEVL-S-REAP PIC 9(03) COMP-3.
              10 SLIG28-TOT-FUT-DEVL-N-REAP PIC 9(03) COMP-3.
              10 SLIG28-TOT-FUT-BAIX        PIC 9(03) COMP-3.
              10 SLIG28-TOT-FUT-BAX-ANT     PIC 9(03) COMP-3.
      *
      *    IDENTIFICA NOME DO EMITENTE COLETADO DO SERASA
           05 SLIG28-NOME-EMITT-SERASA      PIC X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG28-CGC-CPF-EMITT-ST       PIC X(009).
           05 SLIG28-FILIAL-EMITT-ST        PIC X(004).
           05 SLIG28-CONTROLE-EMITT-ST      PIC 9(002) COMP-3.
           05 SLIG28-CGC-CPF-CLI-ST         PIC X(009).
           05 SLIG28-FILIAL-CLI-ST          PIC X(004).
           05 SLIG28-CONTROLE-CLI-ST        PIC 9(002) COMP-3.
           05 SLIG28-FILLER                 PIC X(50).
