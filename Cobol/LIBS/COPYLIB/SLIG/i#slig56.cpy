      ***===========================================================***
      *** NOME INC                                     LENGTH=00308 ***
      *** I#SLIG56                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO REGS.SELECIONADOS CHEQUES - EMITENTE   ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-SELECIONADO.
           03  SLIG56-CTRL.
               05  SLIG56-CGC-CPF-CTRL-ST.
                   07  SLIG56-CGC-CPF-CTRL       PIC 9(009).
               05  SLIG56-PRODUTO-CTRL           PIC X(001).
               05  SLIG56-CARTEIRA-CTRL          PIC 9(001).
               05  SLIG56-AGENCIA-EMITT-CTRL     PIC 9(004).
               05  SLIG56-CONTA-EMITT-CTRL       PIC 9(013).
               05  SLIG56-NUM-CHEQUE-CTRL        PIC 9(007).
               05  SLIG56-DT-ENTRADA-CTRL        PIC 9(008).
               05  SLIG56-DT-CONTABIL-CTRL       PIC 9(008).
           03  SLIG56-IDENT-DOCTO.
               05  SLIG56-BANCO-EMITT            PIC 9(003).
               05  FILLER                        PIC X(002).
               05  SLIG56-AGEN-EMITT             PIC 9(004).
               05  FILLER                        PIC X(001).
               05  SLIG56-CTA-EMITT              PIC 9(013).
               05  FILLER                        PIC X(001).
               05  SLIG56-NUM-CHEQUE-EMITT       PIC 9(007).
               05  FILLER                        PIC X(004).
           03  SLIG-PERIODO.
               05  SLIG56-DT-PER-INI             PIC 9(008).
               05  SLIG56-DT-PER-FIM             PIC 9(008).
           03  SLIG-REG-CHEQUES.
               05 SLIG56-CGC-CPF-EMITT           PIC 9(009) COMP-3.
               05 SLIG56-FILIAL-EMITT            PIC 9(004) COMP-3.
               05 SLIG56-CONTROLE-EMITT          PIC 9(002) COMP-3.
               05 SLIG56-TIPO-PESSOA-EMITT       PIC X(001).
               05 SLIG56-BCO-EMITT               PIC 9(003) COMP-3.
               05 SLIG56-AGENCIA-EMITT           PIC 9(005) COMP-3.
               05 SLIG56-CONTA-EMITT             PIC 9(013) COMP-3.
               05 SLIG56-NUM-CHEQUE              PIC 9(007) COMP-3.
               05 SLIG56-DIG-CHEQUE              PIC X(001).
               05 SLIG56-NUM-LOTE                PIC 9(007) COMP-3.
               05 SLIG56-CARTEIRA                PIC 9(003) COMP-3.
                  88 SLIG56-CARTEIRA-SIMPLES     VALUE 1.
                  88 SLIG56-CARTEIRA-CAUCIONADA  VALUE 2.
                  88 SLIG56-CARTEIRA-DESCONTO    VALUE 3.
               05 SLIG56-NOME-EMITT              PIC X(040).
               05 SLIG56-BCO-CLI                 PIC 9(003) COMP-3.
               05 SLIG56-AGENCIA-CLI             PIC 9(005) COMP-3.
               05 SLIG56-CONTA-CLI               PIC 9(013) COMP-3.
               05 SLIG56-CGC-CPF-CLI             PIC 9(009) COMP-3.
               05 SLIG56-FILIAL-CLI              PIC 9(004) COMP-3.
               05 SLIG56-CONTROLE-CLI            PIC 9(002) COMP-3.
               05 SLIG56-TIPO-PESSOA-CLI         PIC X(001).
               05 SLIG56-NOME-CLI                PIC X(040).
               05 SLIG56-DT-DEPOSITO             PIC 9(008).
               05 SLIG56-DT-DEPOSITO-RED REDEFINES SLIG56-DT-DEPOSITO.
                  10 SLIG56-ANO-DEPOSITO         PIC 9(004).
                  10 SLIG56-MES-DEPOSITO         PIC 9(002).
                  10 SLIG56-DIA-DEPOSITO         PIC 9(002).
               05 SLIG56-DT-CONTABIL             PIC 9(008).
               05 SLIG56-DT-CONTABIL-RED REDEFINES SLIG56-DT-CONTABIL.
                  10 SLIG56-ANO-CONTABIL         PIC 9(004).
                  10 SLIG56-MES-CONTABIL         PIC 9(002).
                  10 SLIG56-DIA-CONTABIL         PIC 9(002).
               05 SLIG56-DT-ENTRADA-ORIGEM       PIC 9(008).
               05 SLIG56-DT-ENTRADA-ORIGEM-RED REDEFINES
                  SLIG56-DT-ENTRADA-ORIGEM.
                  10 SLIG56-ANO-ENTRADA-ORIGEM   PIC 9(004).
                  10 SLIG56-MES-ENTRADA-ORIGEM   PIC 9(002).
                  10 SLIG56-DIA-ENTRADA-ORIGEM   PIC 9(002).
               05 SLIG56-DT-SITUACAO             PIC 9(008).
               05 SLIG56-DT-SITUACAO-RED REDEFINES SLIG56-DT-SITUACAO.
                  10 SLIG56-ANO-SITUACAO         PIC 9(004).
                  10 SLIG56-MES-SITUACAO         PIC 9(002).
                  10 SLIG56-DIA-SITUACAO         PIC 9(002).
               05 SLIG56-CTPO-CHEQ               PIC 9(003) COMP-3.
                  88 SLIG56-CTPO-CHEQ-NORML      VALUE 1.
                  88 SLIG56-CTPO-CHEQ-BAIXA      VALUE 2.
                  88 SLIG56-CTPO-CHEQ-COMPE      VALUE 3.
                  88 SLIG56-CTPO-CHEQ-DEVOL      VALUE 4.
                  88 SLIG56-CTPO-CHEQ-REAP-PG    VALUE 5.
                  88 SLIG56-CTPO-CHEQ-REAP-DEV   VALUE 6.
               05 SLIG56-VL-PRINC-CHEQ           PIC 9(013)V99 COMP-3.
               05 SLIG56-BCO-DIR-REGIONAL        PIC 9(003) COMP-3.
               05 SLIG56-DIR-REGIONAL            PIC 9(005) COMP-3.
               05 SLIG56-BCO-GERC-REGIONAL       PIC 9(003) COMP-3.
               05 SLIG56-GERC-REGIONAL           PIC 9(005) COMP-3.
               05 SLIG56-GRP-ECONOMICO-EMITT     PIC 9(009) COMP-3.
               05 SLIG56-GRP-ECONOMICO-CLI       PIC 9(009) COMP-3.
               05 SLIG56-CODI-DEVOL-CHQ          PIC 9(002).

      * === CAMPOS DUPLICADOS PARA STRING ===
               05 SLIG56-CGC-CPF-EMITT-ST        PIC X(009).
               05 SLIG56-FILIAL-EMITT-ST         PIC X(004).
               05 SLIG56-CONTROLE-EMITT-ST       PIC 9(002) COMP-3.
               05 SLIG56-CGC-CPF-CLI-ST          PIC X(009).
               05 SLIG56-FILIAL-CLI-ST           PIC X(004).
               05 SLIG56-CONTROLE-CLI-ST         PIC 9(002) COMP-3.
               05 SLIG56-FILLER                  PIC X(005).
