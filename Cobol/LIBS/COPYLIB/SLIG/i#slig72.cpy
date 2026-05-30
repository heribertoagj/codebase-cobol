      ***===========================================================***
      *** NOME INC                                     LENGTH=00322 ***
      *** I#SLIG72                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO REGS.SELECIONADOS CHEQUES - EMITENTE   ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-SELECIONADO.
           03  SLIG72-CTRL.
               05  SLIG72-CGC-CPF-CTRL-ST.
                   07  SLIG72-CGC-CPF-CTRL       PIC 9(009).
               05  SLIG72-PRODUTO-CTRL           PIC X(001).
               05  SLIG72-CARTEIRA-CTRL          PIC 9(001).
               05  SLIG72-AGENCIA-EMITT-CTRL     PIC 9(004).
               05  SLIG72-CONTA-EMITT-CTRL       PIC 9(013).
               05  SLIG72-NUM-CHEQUE-CTRL        PIC 9(007).
               05  SLIG72-DT-ENTRADA-CTRL        PIC 9(008).
               05  SLIG72-DT-CONTABIL-CTRL       PIC 9(008).
           03  SLIG72-IDENT-DOCTO.
               05  SLIG72-BANCO-EMITT            PIC 9(003).
               05  FILLER                        PIC X(002).
               05  SLIG72-AGEN-EMITT             PIC 9(004).
               05  FILLER                        PIC X(001).
               05  SLIG72-CTA-EMITT              PIC 9(013).
               05  FILLER                        PIC X(001).
               05  SLIG72-NUM-CHEQUE-EMITT       PIC 9(007).
               05  FILLER                        PIC X(004).
           03  SLIG-PERIODO.
               05  SLIG72-DT-PER-INI             PIC 9(008).
               05  SLIG72-DT-PER-FIM             PIC 9(008).
           03  SLIG72-VINCULACAO.
               05 SLIG72-DIR-REG                 PIC 9(005) COMP-3.
               05 SLIG72-GER-REG                 PIC 9(005) COMP-3.
               05 SLIG72-GRP-ECONM               PIC 9(009) COMP-3.
               05 SLIG72-AGENCIA                 PIC 9(005) COMP-3.
           03  SLIG-REG-CHEQUES.
               05 SLIG72-CGC-CPF-EMITT           PIC 9(009) COMP-3.
               05 SLIG72-FILIAL-EMITT            PIC 9(004) COMP-3.
               05 SLIG72-CONTROLE-EMITT          PIC 9(002) COMP-3.
               05 SLIG72-TIPO-PESSOA-EMITT       PIC X(001).
               05 SLIG72-BCO-EMITT               PIC 9(003) COMP-3.
               05 SLIG72-AGENCIA-EMITT           PIC 9(005) COMP-3.
               05 SLIG72-CONTA-EMITT             PIC 9(013) COMP-3.
               05 SLIG72-NUM-CHEQUE              PIC 9(007) COMP-3.
               05 SLIG72-DIG-CHEQUE              PIC X(001).
               05 SLIG72-NUM-LOTE                PIC 9(007) COMP-3.
               05 SLIG72-CARTEIRA                PIC 9(003) COMP-3.
                  88 SLIG72-CARTEIRA-SIMPLES     VALUE 1.
                  88 SLIG72-CARTEIRA-CAUCIONADA  VALUE 2.
                  88 SLIG72-CARTEIRA-DESCONTO    VALUE 3.
               05 SLIG72-NOME-EMITT              PIC X(040).
               05 SLIG72-BCO-CLI                 PIC 9(003) COMP-3.
               05 SLIG72-AGENCIA-CLI             PIC 9(005) COMP-3.
               05 SLIG72-CONTA-CLI               PIC 9(013) COMP-3.
               05 SLIG72-CGC-CPF-CLI             PIC 9(009) COMP-3.
               05 SLIG72-FILIAL-CLI              PIC 9(004) COMP-3.
               05 SLIG72-CONTROLE-CLI            PIC 9(002) COMP-3.
               05 SLIG72-TIPO-PESSOA-CLI         PIC X(001).
               05 SLIG72-NOME-CLI                PIC X(040).
               05 SLIG72-DT-DEPOSITO             PIC 9(008).
               05 SLIG72-DT-DEPOSITO-RED REDEFINES SLIG72-DT-DEPOSITO.
                  10 SLIG72-ANO-DEPOSITO         PIC 9(004).
                  10 SLIG72-MES-DEPOSITO         PIC 9(002).
                  10 SLIG72-DIA-DEPOSITO         PIC 9(002).
               05 SLIG72-DT-CONTABIL             PIC 9(008).
               05 SLIG72-DT-CONTABIL-RED REDEFINES SLIG72-DT-CONTABIL.
                  10 SLIG72-ANO-CONTABIL         PIC 9(004).
                  10 SLIG72-MES-CONTABIL         PIC 9(002).
                  10 SLIG72-DIA-CONTABIL         PIC 9(002).
               05 SLIG72-DT-ENTRADA-ORIGEM       PIC 9(008).
               05 SLIG72-DT-ENTRADA-ORIGEM-RED REDEFINES
                  SLIG72-DT-ENTRADA-ORIGEM.
                  10 SLIG72-ANO-ENTRADA-ORIGEM   PIC 9(004).
                  10 SLIG72-MES-ENTRADA-ORIGEM   PIC 9(002).
                  10 SLIG72-DIA-ENTRADA-ORIGEM   PIC 9(002).
               05 SLIG72-DT-SITUACAO             PIC 9(008).
               05 SLIG72-DT-SITUACAO-RED REDEFINES SLIG72-DT-SITUACAO.
                  10 SLIG72-ANO-SITUACAO         PIC 9(004).
                  10 SLIG72-MES-SITUACAO         PIC 9(002).
                  10 SLIG72-DIA-SITUACAO         PIC 9(002).
               05 SLIG72-CTPO-CHEQ               PIC 9(003) COMP-3.
                  88 SLIG72-CTPO-CHEQ-NORML      VALUE 1.
                  88 SLIG72-CTPO-CHEQ-BAIXA      VALUE 2.
                  88 SLIG72-CTPO-CHEQ-COMPE      VALUE 3.
                  88 SLIG72-CTPO-CHEQ-DEVOL      VALUE 4.
                  88 SLIG72-CTPO-CHEQ-REAP-PG    VALUE 5.
                  88 SLIG72-CTPO-CHEQ-REAP-DEV   VALUE 6.
               05 SLIG72-VL-PRINC-CHEQ           PIC 9(013)V99 COMP-3.
               05 SLIG72-BCO-DIR-REGIONAL        PIC 9(003) COMP-3.
               05 SLIG72-DIR-REGIONAL            PIC 9(005) COMP-3.
               05 SLIG72-BCO-GERC-REGIONAL       PIC 9(003) COMP-3.
               05 SLIG72-GERC-REGIONAL           PIC 9(005) COMP-3.
               05 SLIG72-GRP-ECONOMICO-EMITT     PIC 9(009) COMP-3.
               05 SLIG72-GRP-ECONOMICO-CLI       PIC 9(009) COMP-3.
               05 SLIG72-CGC-CPF-EMITT-ST        PIC X(009).
               05 SLIG72-FILIAL-EMITT-ST         PIC X(004).
               05 SLIG72-CONTROLE-EMITT-ST       PIC 9(002) COMP-3.
               05 SLIG72-CGC-CPF-CLI-ST          PIC X(009).
               05 SLIG72-FILIAL-CLI-ST           PIC X(004).
               05 SLIG72-CONTROLE-CLI-ST         PIC 9(002) COMP-3.
               05 SLIG72-FILLER                  PIC X(007).
