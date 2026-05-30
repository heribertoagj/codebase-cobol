      ***===========================================================***
      *** NOME INC                                     LENGTH=0308  ***
      *** I#SLIG62                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO REGS.SELECIONADOS TITULOS-VENDOR-CHEQUES**
      **             PARA EMISSAO RELATORIO ANALISE PERFOMANCE       **
      ***===========================================================***
       01 REG-SELECIONADO.
          03 SLIG62-CTRL.
             05 SLIG62-CGC-CPF-CTRL-ST.
                07 SLIG62-CGC-CPF-CTRL           PIC 9(009).
             05 SLIG62-PRODUTO-CTRL              PIC X(001).
             05 SLIG62-CARTEIRA-CTRL             PIC 9(001).
             05 SLIG62-CTRL-TIT-VND.
                07 SLIG62-CD-PRODUTO-CTRL        PIC 9(002).
                07 SLIG62-CD-CNEGOC-CTRL         PIC 9(011).
                07 SLIG62-CD-NOSNRO-CTRL         PIC 9(011).
             05 SLIG62-CTRL-CHQ REDEFINES SLIG62-CTRL-TIT-VND.
                07 SLIG62-AGENCIA-EMITT-CTRL     PIC 9(004).
                07 SLIG62-CONTA-EMITT-CTRL       PIC 9(013).
                07 SLIG62-NUM-CHEQUE-CTRL        PIC 9(007).
             05 SLIG62-DT-ENTRADA-CTRL           PIC 9(008).
             05 SLIG62-DT-VENCTO-CTRL            PIC 9(008).
          03 SLIG62-IDENT-DOCTO                  PIC X(035).
          03 SLIG62-IDENT-DOCTO-TIT REDEFINES SLIG62-IDENT-DOCTO.
             05  SLIG62-CD-PRODUTO-CLI           PIC 9(002).
             05  FILLER                          PIC X(002).
             05  SLIG62-AGENCIA-CLI              PIC 9(004).
             05  FILLER                          PIC X(002).
             05  SLIG62-CONTA-CLI                PIC 9(007).
             05  FILLER                          PIC X(002).
             05  SLIG62-NUM-TIT                  PIC 9(011).
             05  FILLER                          PIC X(005).
          03 SLIG62-IDENT-DOCTO-VND REDEFINES SLIG62-IDENT-DOCTO.
             05  SLIG62-CPRODT-CLI               PIC 9(003).
             05  FILLER                          PIC X(001).
             05  SLIG62-CFAML-CLI                PIC 9(001).
             05  FILLER                          PIC X(001).
             05  SLIG62-CCONTR-CLI               PIC 9(009).
             05  FILLER                          PIC X(001).
             05  SLIG62-CSEQ-CLI                 PIC 9(003).
             05  FILLER                          PIC X(001).
             05  SLIG62-CPARCELA-CLI             PIC 9(003).
             05  FILLER                          PIC X(001).
             05  SLIG62-NUM-DOCTO                PIC 9(011).
          03 SLIG62-IDENT-DOCTO-CHQ REDEFINES SLIG62-IDENT-DOCTO.
             05  SLIG62-BANCO-EMITT              PIC 9(003).
             05  FILLER                          PIC X(002).
             05  SLIG62-AGEN-EMITT               PIC 9(004).
             05  FILLER                          PIC X(001).
             05  SLIG62-CTA-EMITT                PIC 9(013).
             05  FILLER                          PIC X(001).
             05  SLIG62-NUM-CHEQUE-EMITT         PIC 9(007).
             05  FILLER                          PIC X(004).
          03 SLIG62-PERIODOS.
             05  SLIG62-DT-PER-INI               PIC 9(008).
             05  SLIG62-DT-PER-FIM               PIC 9(008).
          03 SLIG62-REG-PRODUTOS                 PIC X(236).
          03 SLIG62-REG-TITULOS REDEFINES SLIG62-REG-PRODUTOS.
             05 SLIG62-TIT-BCO-CLIENTE           PIC 9(003) COMP-3.
             05 SLIG62-TIT-AGENCIA-CLIENTE       PIC 9(005) COMP-3.
             05 SLIG62-TIT-CONTA-CLIENTE         PIC 9(013) COMP-3.
             05 SLIG62-TIT-CARTEIRA              PIC 9(003) COMP-3.
             05 SLIG62-TIT-CCLI-COBR             PIC 9(009) COMP-3.
             05 SLIG62-TIT-CGC-CPF-CLIENTE       PIC 9(009) COMP-3.
             05 SLIG62-TIT-FILIAL-CLIENTE        PIC 9(005) COMP-3.
             05 SLIG62-TIT-CONTROLE-CLIENTE      PIC 9(002).
             05 SLIG62-TIT-BCO-SACADO            PIC 9(003) COMP-3.
             05 SLIG62-TIT-AGENCIA-SACADO        PIC 9(005) COMP-3.
             05 SLIG62-TIT-CONTA-SACADO          PIC 9(013) COMP-3.
             05 SLIG62-TIT-CGC-CPF-SACADO        PIC 9(009) COMP-3.
             05 SLIG62-TIT-FILIAL-SACADO         PIC 9(005) COMP-3.
             05 SLIG62-TIT-CONTROLE-SACADO       PIC 9(002).
             05 SLIG62-TIT-VL-PRINC-PROD         PIC 9(017) COMP-3.
             05 SLIG62-TIT-VL-PAGO-PROD          PIC 9(015) COMP-3.
             05 SLIG62-TIT-VL-DESC-ABAT-PROD     PIC 9(015) COMP-3.
             05 SLIG62-TIT-DT-VENCTO-PROD        PIC 9(009) COMP-3.
             05 SLIG62-TIT-DT-PAGTO-PROD         PIC 9(009) COMP-3.
             05 SLIG62-TIT-DT-ENTRADA-ORIGEM     PIC 9(009) COMP-3.
             05 SLIG62-TIT-DT-ENTRADA-SISTEMA    PIC 9(009) COMP-3.
             05 SLIG62-TIT-DT-VENCTO-ANTERIOR    PIC 9(009) COMP-3.
             05 SLIG62-TIT-CD-PRODUTO            PIC 9(002).
             05 SLIG62-TIT-CD-CNEGOC             PIC 9(011) COMP-3.
             05 SLIG62-TIT-CD-NOSNRO             PIC 9(011) COMP-3.
             05 SLIG62-TIT-CD-SEQUENCIA          PIC 9(003) COMP-3.
             05 SLIG62-TIT-CD-COD-NTAX           PIC 9(005) COMP-3.
             05 SLIG62-TIT-CD-OCO-REM-L          PIC 9(003) COMP-3.
             05 SLIG62-TIT-CD-MOT-REM-SIS        PIC 9(003) COMP-3.
             05 SLIG62-TIT-CD-ESPECIE            PIC 9(003) COMP-3.
             05 SLIG62-TIT-CD-ORIGEM             PIC 9(003) COMP-3.
             05 SLIG62-TIT-DT-INICIO-PER         PIC 9(009) COMP-3.
             05 SLIG62-TIT-DT-FINAL-PER          PIC 9(009) COMP-3.
             05 SLIG62-TIT-CGC-CPF-CLIENTE-ST    PIC X(009).
             05 SLIG62-TIT-FILIAL-CLIENTE-ST     PIC X(005).
             05 SLIG62-TIT-CONTROLE-CLIENTE-ST   PIC 9(002).
             05 SLIG62-TIT-CGC-CPF-SACADO-ST     PIC X(009).
             05 SLIG62-TIT-FILIAL-SACADO-ST      PIC X(005).
             05 SLIG62-TIT-CONTROLE-SACADO-ST    PIC 9(002).
             05  FILLER                          PIC X(066).
          03 SLIG62-REG-VENDOR REDEFINES SLIG62-REG-PRODUTOS.
             05 SLIG62-VND-CGC-CPF-COMP          PIC 9(009) COMP-3.
             05 SLIG62-VND-FILIAL-COMP           PIC 9(004) COMP-3.
             05 SLIG62-VND-CONTROLE-COMP         PIC 9(002) COMP-3.
             05 SLIG62-VND-TIPO-PESSOA-COMP      PIC X(001).
             05 SLIG62-VND-BCO-COMP              PIC 9(003) COMP-3.
             05 SLIG62-VND-AGENCIA-COMP          PIC 9(005) COMP-3.
             05 SLIG62-VND-CONTA-COMP            PIC 9(013) COMP-3.
             05 SLIG62-VND-NOME-COMP             PIC X(040).
             05 SLIG62-VND-CPRODT                PIC 9(003) COMP-3.
             05 SLIG62-VND-CFAML                 PIC 9(001) COMP-3.
             05 SLIG62-VND-CCONTR                PIC 9(009) COMP-3.
             05 SLIG62-VND-CSEQ                  PIC 9(003) COMP-3.
             05 SLIG62-VND-CPARCELA              PIC 9(003) COMP-3.
             05 SLIG62-VND-NUM-PAGTO             PIC 9(009) COMP-3.
             05 SLIG62-VND-NOSSO-NUMERO          PIC 9(011) COMP-3.
             05 SLIG62-VND-CGC-CPF-FORN          PIC 9(009) COMP-3.
             05 SLIG62-VND-FILIAL-FORN           PIC 9(004) COMP-3.
             05 SLIG62-VND-CONTROLE-FORN         PIC 9(002) COMP-3.
             05 SLIG62-VND-TIPO-PESSOA-FORN      PIC X(001).
             05 SLIG62-VND-BCO-FORN              PIC 9(003) COMP-3.
             05 SLIG62-VND-AGENCIA-FORN          PIC 9(005) COMP-3.
             05 SLIG62-VND-CONTA-FORN            PIC 9(013) COMP-3.
             05 SLIG62-VND-NOME-FORN             PIC X(040).
             05 SLIG62-VND-DT-VENCTO-PARC        PIC 9(008).
             05 SLIG62-VND-DT-VENCTO-PARC-RED REDEFINES
                SLIG62-VND-DT-VENCTO-PARC.
                07 SLIG62-VND-ANO-VENCTO-PARC    PIC 9(004).
                07 SLIG62-VND-MES-VENCTO-PARC    PIC 9(002).
                07 SLIG62-VND-DIA-VENCTO-PARC    PIC 9(002).
             05 SLIG62-VND-DT-PAGTO-PARC         PIC 9(008).
             05 SLIG62-VND-DT-PAGTO-PARC-RED REDEFINES
                SLIG62-VND-DT-PAGTO-PARC.
                07 SLIG62-VND-ANO-PAGTO-PARC     PIC 9(004).
                07 SLIG62-VND-MES-PAGTO-PARC     PIC 9(002).
                07 SLIG62-VND-DIA-PAGTO-PARC     PIC 9(002).
             05 SLIG62-VND-CPTO-SIT-PARC         PIC 9(003) COMP-3.
                88 SLIG62-VND-CTPO-PAGTO-NORMAL  VALUE 1.
                88 SLIG62-VND-CTPO-PAGTO-ANTECI  VALUE 2.
                88 SLIG62-VND-CTPO-BAIXA-NORMAL  VALUE 3.
                88 SLIG62-VND-CTPO-BAIXA-ANTECI  VALUE 4.
                88 SLIG62-VND-CTPO-BAIXA-EXCLUS  VALUE 5.
                88 SLIG62-VND-CTPO-SIT-PARC-VENC VALUE 6.
                88 SLIG62-VND-CTPO-SIT-PARC-FUTU VALUE 7.
             05 SLIG62-VND-DT-ENTRADA-ORIGEM     PIC 9(008).
             05 SLIG62-VND-DT-ENTRADA-ORIGEM-R   REDEFINES
                SLIG62-VND-DT-ENTRADA-ORIGEM.
                07 SLIG62-VND-ANO-ENTRADA-ORIGEM PIC 9(004).
                07 SLIG62-VND-MES-ENTRADA-ORIGEM PIC 9(002).
                07 SLIG62-VND-DIA-ENTRADA-ORIGEM PIC 9(002).
             05 SLIG62-VND-VL-PRINC-PARC         PIC 9(013)V99 COMP-3.
             05 SLIG62-VND-IND-PAGTO             PIC X(001).
                88 SLIG62-VND-IND-PAGTO-COMP     VALUE '1'.
                88 SLIG62-VND-IND-PAGTO-FORN     VALUE '2'.
             05 SLIG62-VND-BCO-DIR-REGIONAL      PIC 9(003) COMP-3.
             05 SLIG62-VND-DIR-REGIONAL          PIC 9(005) COMP-3.
             05 SLIG62-VND-BCO-GERC-REGIONAL     PIC 9(003) COMP-3.
             05 SLIG62-VND-GERC-REGIONAL         PIC 9(005) COMP-3.
             05 SLIG62-VND-GRP-ECONOMICO-COMP    PIC 9(009) COMP-3.
             05 SLIG62-VND-GRP-ECONOMICO-FORN    PIC 9(009) COMP-3.
             05 SLIG62-VND-CGC-CPF-COMP-ST       PIC X(009).
             05 SLIG62-VND-FILIAL-COMP-ST        PIC X(004).
             05 SLIG62-VND-CONTROLE-COMP-ST      PIC 9(002) COMP-3.
             05 SLIG62-VND-CGC-CPF-FORN-ST       PIC X(009).
             05 SLIG62-VND-FILIAL-FORN-ST        PIC X(004).
             05 SLIG62-VND-CONTROLE-FORN-ST      PIC 9(002) COMP-3.
             05 FILLER                           PIC X(002).
          03 SLIG62-REG-CHEQUES REDEFINES SLIG62-REG-PRODUTOS.
             05 SLIG62-CHQ-CGC-CPF-EMITT         PIC 9(009) COMP-3.
             05 SLIG62-CHQ-FILIAL-EMITT          PIC 9(004) COMP-3.
             05 SLIG62-CHQ-CONTROLE-EMITT        PIC 9(002) COMP-3.
             05 SLIG62-CHQ-TIPO-PESSOA-EMITT     PIC X(001).
             05 SLIG62-CHQ-BCO-EMITT             PIC 9(003) COMP-3.
             05 SLIG62-CHQ-AGENCIA-EMITT         PIC 9(005) COMP-3.
             05 SLIG62-CHQ-CONTA-EMITT           PIC 9(013) COMP-3.
             05 SLIG62-CHQ-NUM-CHEQUE            PIC 9(007) COMP-3.
             05 SLIG62-CHQ-DIG-CHEQUE            PIC X(001).
             05 SLIG62-CHQ-NUM-LOTE              PIC 9(007) COMP-3.
             05 SLIG62-CHQ-CARTEIRA              PIC 9(003) COMP-3.
                88 SLIG62-CHQ-CARTEIRA-SIMPLES    VALUE 1.
                88 SLIG62-CHQ-CARTEIRA-CAUCIONADA VALUE 2.
                88 SLIG62-CHQ-CARTEIRA-DESCONTO   VALUE 3.
             05 SLIG62-CHQ-NOME-EMITT            PIC X(040).
             05 SLIG62-CHQ-BCO-CLI               PIC 9(003) COMP-3.
             05 SLIG62-CHQ-AGENCIA-CLI           PIC 9(005) COMP-3.
             05 SLIG62-CHQ-CONTA-CLI             PIC 9(013) COMP-3.
             05 SLIG62-CHQ-CGC-CPF-CLI           PIC 9(009) COMP-3.
             05 SLIG62-CHQ-FILIAL-CLI            PIC 9(004) COMP-3.
             05 SLIG62-CHQ-CONTROLE-CLI          PIC 9(002) COMP-3.
             05 SLIG62-CHQ-TIPO-PESSOA-CLI       PIC X(001).
             05 SLIG62-CHQ-NOME-CLI              PIC X(040).
             05 SLIG62-CHQ-DT-DEPOSITO           PIC 9(008).
             05 SLIG62-CHQ-DT-DEPOSITO-RED REDEFINES
                SLIG62-CHQ-DT-DEPOSITO.
                10 SLIG62-CHQ-ANO-DEPOSITO       PIC 9(004).
                10 SLIG62-CHQ-MES-DEPOSITO       PIC 9(002).
                10 SLIG62-CHQ-DIA-DEPOSITO       PIC 9(002).
             05 SLIG62-CHQ-DT-CONTABIL           PIC 9(008).
             05 SLIG62-CHQ-DT-CONTABIL-RED REDEFINES
                SLIG62-CHQ-DT-CONTABIL.
                10 SLIG62-CHQ-ANO-CONTABIL       PIC 9(004).
                10 SLIG62-CHQ-MES-CONTABIL       PIC 9(002).
                10 SLIG62-CHQ-DIA-CONTABIL       PIC 9(002).
             05 SLIG62-CHQ-DT-ENTRADA-ORIGEM     PIC 9(008).
             05 SLIG62-CHQ-DT-ENTRADA-ORIGEM-R   REDEFINES
                SLIG62-CHQ-DT-ENTRADA-ORIGEM.
                10 SLIG62-CHQ-ANO-ENTRADA-ORIGEM PIC 9(004).
                10 SLIG62-CHQ-MES-ENTRADA-ORIGEM PIC 9(002).
                10 SLIG62-CHQ-DIA-ENTRADA-ORIGEM PIC 9(002).
             05 SLIG62-CHQ-DT-SITUACAO           PIC 9(008).
             05 SLIG62-CHQ-DT-SITUACAO-RED REDEFINES
                SLIG62-CHQ-DT-SITUACAO.
                10 SLIG62-CHQ-ANO-SITUACAO       PIC 9(004).
                10 SLIG62-CHQ-MES-SITUACAO       PIC 9(002).
                10 SLIG62-CHQ-DIA-SITUACAO       PIC 9(002).
             05 SLIG62-CHQ-CTPO-CHEQ             PIC 9(003) COMP-3.
                88 SLIG62-CHQ-CTPO-CHEQ-NORML    VALUE 1.
                88 SLIG62-CHQ-CTPO-CHEQ-BAIXA    VALUE 2.
                88 SLIG62-CHQ-CTPO-CHEQ-COMPE    VALUE 3.
                88 SLIG62-CHQ-CTPO-CHEQ-DEVOL    VALUE 4.
                88 SLIG62-CHQ-CTPO-CHEQ-REAP-PG  VALUE 5.
                88 SLIG62-CHQ-CTPO-CHEQ-REAP-DEV VALUE 6.
             05 SLIG62-CHQ-VL-PRINC-CHEQ         PIC 9(013)V99 COMP-3.
             05 SLIG62-CHQ-BCO-DIR-REGIONAL      PIC 9(003) COMP-3.
             05 SLIG62-CHQ-DIR-REGIONAL          PIC 9(005) COMP-3.
             05 SLIG62-CHQ-BCO-GERC-REGIONAL     PIC 9(003) COMP-3.
             05 SLIG62-CHQ-GERC-REGIONAL         PIC 9(005) COMP-3.
             05 SLIG62-CHQ-GRP-ECONOMICO-EMITT   PIC 9(009) COMP-3.
             05 SLIG62-CHQ-GRP-ECONOMICO-CLI     PIC 9(009) COMP-3.
             05 SLIG62-CODI-DEVOL-CHQ            PIC 9(002).
             05 SLIG62-CHQ-CGC-CPF-EMITT-ST      PIC X(009).
             05 SLIG62-CHQ-FILIAL-EMITT-ST       PIC X(004).
             05 SLIG62-CHQ-CONTROLE-EMITT-ST     PIC 9(002) COMP-3.
             05 SLIG62-CHQ-CGC-CPF-CLI-ST        PIC X(009).
             05 SLIG62-CHQ-FILIAL-CLI-ST         PIC X(004).
             05 SLIG62-CHQ-CONTROLE-CLI-ST       PIC 9(002) COMP-3.
             05 FILLER                           PIC X(005).
