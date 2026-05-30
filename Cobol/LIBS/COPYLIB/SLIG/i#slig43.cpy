      ***===========================================================***
      *** NOME INC                                     LENGTH=00300 ***
      *** I#SLIG43                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - CONSOLIDADO EMITENTE          ***
      ***                                                           ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA EMITENTE E CLIENTE E CONTA CLIENTE        ***
      ***===========================================================***
      *
       01  REG-CHEQUE-CONSOLIDADO-EMITT.
           05 SLIG43-CGC-CPF-EMITT          PIC 9(009) COMP-3.
           05 SLIG43-TIPO-PESSOA-EMITT      PIC X(001).
           05 SLIG43-BCO-EMITT              PIC 9(003) COMP-3.
           05 SLIG43-AGENCIA-EMITT          PIC 9(005) COMP-3.
           05 SLIG43-CONTA-EMITT            PIC 9(013) COMP-3.
           05 SLIG43-NOME-EMITT             PIC X(040).
           05 SLIG43-CARTEIRA               PIC 9(003) COMP-3.
              88 SLIG43-CARTEIRA-SIMPLES    VALUE 1.
              88 SLIG43-CARTEIRA-CAUCIONADA VALUE 2.
              88 SLIG43-CARTEIRA-DESCONTO   VALUE 3.
           05 SLIG43-GRP-ECONOMICO-EMITT    PIC 9(009) COMP-3.
           05 SLIG43-CGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIG43-BCO-CLI                PIC 9(003) COMP-3.
           05 SLIG43-AGENCIA-CLI            PIC 9(005) COMP-3.
           05 SLIG43-TIPO-PESSOA-CLI        PIC X(001).
           05 SLIG43-NOME-CLI               PIC X(040).
           05 SLIG43-BCO-DIR-REGIONAL       PIC 9(003) COMP-3.
           05 SLIG43-DIR-REGIONAL           PIC 9(005) COMP-3.
           05 SLIG43-BCO-GERC-REGIONAL      PIC 9(003) COMP-3.
           05 SLIG43-GERC-REGIONAL          PIC 9(005) COMP-3.
           05 SLIG43-GRP-ECONOMICO-CLI      PIC 9(009) COMP-3.
           05 SLIG43-VCTO-PER-PG            PIC 9(013)V99 COMP-3.
           05 SLIG43-VCTO-PER-NAO-PG        PIC 9(013)V99 COMP-3.
           05 SLIG43-VCTO-PER-BXADO         PIC 9(013)V99 COMP-3.
           05 SLIG43-VCTO-APOS-PER-PG       PIC 9(013)V99 COMP-3.
           05 SLIG43-VCTO-APOS-PER-NAO-PG   PIC 9(013)V99 COMP-3.
           05 SLIG43-VCTO-APOS-PER-BXADO    PIC 9(013)V99 COMP-3.
           05 SLIG43-CHEQ-PRIM-DEVLC        PIC 9(013)V99 COMP-3.
           05 SLIG43-CHEQ-REAP              PIC 9(013)V99 COMP-3.
           05 SLIG43-CHEQ-REAP-DEVOL        PIC 9(013)V99 COMP-3.
           05 SLIG43-CHEQ-REAP-PG           PIC 9(013)V99 COMP-3.
           05 SLIG43-CHEQ-DEVOL-ANTER       PIC 9(013)V99 COMP-3.
           05 SLIG43-FILIAL-EMITT           PIC 9(004) COMP-3.
           05 SLIG43-CONTROLE-EMITT         PIC 9(002) COMP-3.
           05 SLIG43-FILIAL-CLI             PIC 9(004) COMP-3.
           05 SLIG43-CONTROLE-CLI           PIC 9(002) COMP-3.
           05 SLIG43-CONTA-CLI              PIC 9(013) COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG43-CGC-CPF-EMITT-ST       PIC X(009).
           05 SLIG43-FILIAL-EMITT-ST        PIC X(004).
           05 SLIG43-CONTROLE-EMITT-ST      PIC 9(002) COMP-3.
           05 SLIG43-CGC-CPF-CLI-ST         PIC X(009).
           05 SLIG43-FILIAL-CLI-ST          PIC X(004).
           05 SLIG43-CONTROLE-CLI-ST        PIC 9(002) COMP-3.
           05 SLIG43-FILLER                 PIC X(034).
