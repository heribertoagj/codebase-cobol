      ***===========================================================***
      *** NOME INC                                     LENGTH=0322  ***
      *** I#SLIG73                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO REGS.SELECIONADOS TITULOS-VENDOR-CHEQUES**
      **             PARA EMISSAO RELATORIO ANALISE PERFOMANCE       **
      ***===========================================================***
       01 REG-SELECIONADO.
          03 SLIG73-CTRL.
             05 SLIG73-CGC-CPF-CTRL-ST.
                07 SLIG73-CGC-CPF-CTRL           PIC 9(009).
             05 SLIG73-PRODUTO-CTRL              PIC X(001).
             05 SLIG73-CARTEIRA-CTRL             PIC 9(001).
             05 SLIG73-CTRL-TIT-VND.
                07 SLIG73-CD-PRODUTO-CTRL        PIC 9(002).
                07 SLIG73-CD-CNEGOC-CTRL         PIC 9(011).
                07 SLIG73-CD-NOSNRO-CTRL         PIC 9(011).
             05 SLIG73-CTRL-CHQ REDEFINES SLIG73-CTRL-TIT-VND.
                07 SLIG73-AGENCIA-EMITT-CTRL     PIC 9(004).
                07 SLIG73-CONTA-EMITT-CTRL       PIC 9(013).
                07 SLIG73-NUM-CHEQUE-CTRL        PIC 9(007).
             05 SLIG73-DT-ENTRADA-CTRL           PIC 9(008).
             05 SLIG73-DT-VENCTO-CTRL            PIC 9(008).
          03 SLIG73-IDENT-DOCTO                  PIC X(035).
          03 SLIG73-IDENT-DOCTO-TIT REDEFINES SLIG73-IDENT-DOCTO.
             05  SLIG73-CD-PRODUTO-CLI           PIC 9(002).
             05  FILLER                          PIC X(002).
             05  SLIG73-AGENCIA-CLI              PIC 9(004).
             05  FILLER                          PIC X(002).
             05  SLIG73-CONTA-CLI                PIC 9(007).
             05  FILLER                          PIC X(002).
             05  SLIG73-NUM-TIT                  PIC 9(011).
             05  FILLER                          PIC X(005).
          03 SLIG73-IDENT-DOCTO-VND REDEFINES SLIG73-IDENT-DOCTO.
             05  SLIG73-CPRODT-CLI               PIC 9(003).
             05  FILLER                          PIC X(001).
             05  SLIG73-CFAML-CLI                PIC 9(001).
             05  FILLER                          PIC X(001).
             05  SLIG73-CCONTR-CLI               PIC 9(009).
             05  FILLER                          PIC X(001).
             05  SLIG73-CSEQ-CLI                 PIC 9(003).
             05  FILLER                          PIC X(001).
             05  SLIG73-CPARCELA-CLI             PIC 9(003).
             05  FILLER                          PIC X(001).
             05  SLIG73-NUM-DOCTO                PIC 9(011).
          03 SLIG73-IDENT-DOCTO-CHQ REDEFINES SLIG73-IDENT-DOCTO.
             05  SLIG73-BANCO-EMITT              PIC 9(003).
             05  FILLER                          PIC X(002).
             05  SLIG73-AGEN-EMITT               PIC 9(004).
             05  FILLER                          PIC X(001).
             05  SLIG73-CTA-EMITT                PIC 9(013).
             05  FILLER                          PIC X(001).
             05  SLIG73-NUM-CHEQUE-EMITT         PIC 9(007).
             05  FILLER                          PIC X(004).
          03 SLIG73-PERIODOS.
             05  SLIG73-DT-PER-INI               PIC 9(008).
             05  SLIG73-DT-PER-FIM               PIC 9(008).
          03 SLIG70-VINCULACAO.
             05 SLIG73-DIR-REG                   PIC 9(005) COMP-3.
             05 SLIG73-GER-REG                   PIC 9(005) COMP-3.
             05 SLIG73-GRP-ECONM                 PIC 9(009) COMP-3.
             05 SLIG73-AGENCIA                   PIC 9(005) COMP-3.
          03 SLIG73-REG-PRODUTOS                 PIC X(236).
          03 SLIG73-REG-TITULOS REDEFINES SLIG73-REG-PRODUTOS.
             05 SLIG73-TIT-BCO-CLIENTE           PIC 9(003) COMP-3.
             05 SLIG73-TIT-AGENCIA-CLIENTE       PIC 9(005) COMP-3.
             05 SLIG73-TIT-CONTA-CLIENTE         PIC 9(013) COMP-3.
             05 SLIG73-TIT-CARTEIRA              PIC 9(003) COMP-3.
             05 SLIG73-TIT-CCLI-COBR             PIC 9(009) COMP-3.
             05 SLIG73-TIT-CGC-CPF-CLIENTE       PIC 9(009) COMP-3.
             05 SLIG73-TIT-FILIAL-CLIENTE        PIC 9(005) COMP-3.
             05 SLIG73-TIT-CONTROLE-CLIENTE      PIC 9(002).
             05 SLIG73-TIT-BCO-SACADO            PIC 9(003) COMP-3.
             05 SLIG73-TIT-AGENCIA-SACADO        PIC 9(005) COMP-3.
             05 SLIG73-TIT-CONTA-SACADO          PIC 9(013) COMP-3.
             05 SLIG73-TIT-CGC-CPF-SACADO        PIC 9(009) COMP-3.
             05 SLIG73-TIT-FILIAL-SACADO         PIC 9(005) COMP-3.
             05 SLIG73-TIT-CONTROLE-SACADO       PIC 9(002).
             05 SLIG73-TIT-VL-PRINC-PROD         PIC 9(017) COMP-3.
             05 SLIG73-TIT-VL-PAGO-PROD          PIC 9(015) COMP-3.
             05 SLIG73-TIT-VL-DESC-ABAT-PROD     PIC 9(015) COMP-3.
             05 SLIG73-TIT-DT-VENCTO-PROD        PIC 9(009) COMP-3.
             05 SLIG73-TIT-DT-PAGTO-PROD         PIC 9(009) COMP-3.
             05 SLIG73-TIT-DT-ENTRADA-ORIGEM     PIC 9(009) COMP-3.
             05 SLIG73-TIT-DT-ENTRADA-SISTEMA    PIC 9(009) COMP-3.
             05 SLIG73-TIT-DT-VENCTO-ANTERIOR    PIC 9(009) COMP-3.
             05 SLIG73-TIT-CD-PRODUTO            PIC 9(002).
             05 SLIG73-TIT-CD-CNEGOC             PIC 9(011) COMP-3.
             05 SLIG73-TIT-CD-NOSNRO             PIC 9(011) COMP-3.
             05 SLIG73-TIT-CD-SEQUENCIA          PIC 9(003) COMP-3.
             05 SLIG73-TIT-CD-COD-NTAX           PIC 9(005) COMP-3.
             05 SLIG73-TIT-CD-OCO-REM-L          PIC 9(003) COMP-3.
             05 SLIG73-TIT-CD-MOT-REM-SIS        PIC 9(003) COMP-3.
             05 SLIG73-TIT-CD-ESPECIE            PIC 9(003) COMP-3.
             05 SLIG73-TIT-CD-ORIGEM             PIC 9(003) COMP-3.
             05 SLIG73-TIT-DT-INICIO-PER         PIC 9(009) COMP-3.
             05 SLIG73-TIT-DT-FINAL-PER          PIC 9(009) COMP-3.
             05 SLIG73-TIT-CGC-CPF-CLIENTE-ST    PIC X(009).
             05 SLIG73-TIT-FILIAL-CLIENTE-ST     PIC X(005).
             05 SLIG73-TIT-CONTROLE-CLIENTE-ST   PIC 9(002).
             05 SLIG73-TIT-CGC-CPF-SACADO-ST     PIC X(009).
             05 SLIG73-TIT-FILIAL-SACADO-ST      PIC X(005).
             05 SLIG73-TIT-CONTROLE-SACADO-ST    PIC 9(002).
             05  FILLER                          PIC X(066).
          03 SLIG73-REG-VENDOR REDEFINES SLIG73-REG-PRODUTOS.
             05 SLIG73-VND-CGC-CPF-COMP          PIC 9(009) COMP-3.
             05 SLIG73-VND-FILIAL-COMP           PIC 9(004) COMP-3.
             05 SLIG73-VND-CONTROLE-COMP         PIC 9(002) COMP-3.
             05 SLIG73-VND-TIPO-PESSOA-COMP      PIC X(001).
             05 SLIG73-VND-BCO-COMP              PIC 9(003) COMP-3.
             05 SLIG73-VND-AGENCIA-COMP          PIC 9(005) COMP-3.
             05 SLIG73-VND-CONTA-COMP            PIC 9(013) COMP-3.
             05 SLIG73-VND-NOME-COMP             PIC X(040).
             05 SLIG73-VND-CPRODT                PIC 9(003) COMP-3.
             05 SLIG73-VND-CFAML                 PIC 9(001) COMP-3.
             05 SLIG73-VND-CCONTR                PIC 9(009) COMP-3.
             05 SLIG73-VND-CSEQ                  PIC 9(003) COMP-3.
             05 SLIG73-VND-CPARCELA              PIC 9(003) COMP-3.
             05 SLIG73-VND-NUM-PAGTO             PIC 9(009) COMP-3.
             05 SLIG73-VND-NOSSO-NUMERO          PIC 9(011) COMP-3.
             05 SLIG73-VND-CGC-CPF-FORN          PIC 9(009) COMP-3.
             05 SLIG73-VND-FILIAL-FORN           PIC 9(004) COMP-3.
             05 SLIG73-VND-CONTROLE-FORN         PIC 9(002) COMP-3.
             05 SLIG73-VND-TIPO-PESSOA-FORN      PIC X(001).
             05 SLIG73-VND-BCO-FORN              PIC 9(003) COMP-3.
             05 SLIG73-VND-AGENCIA-FORN          PIC 9(005) COMP-3.
             05 SLIG73-VND-CONTA-FORN            PIC 9(013) COMP-3.
             05 SLIG73-VND-NOME-FORN             PIC X(040).
             05 SLIG73-VND-DT-VENCTO-PARC        PIC 9(008).
             05 SLIG73-VND-DT-VENCTO-PARC-RED REDEFINES
                SLIG73-VND-DT-VENCTO-PARC.
                07 SLIG73-VND-ANO-VENCTO-PARC    PIC 9(004).
                07 SLIG73-VND-MES-VENCTO-PARC    PIC 9(002).
                07 SLIG73-VND-DIA-VENCTO-PARC    PIC 9(002).
             05 SLIG73-VND-DT-PAGTO-PARC         PIC 9(008).
             05 SLIG73-VND-DT-PAGTO-PARC-RED REDEFINES
                SLIG73-VND-DT-PAGTO-PARC.
                07 SLIG73-VND-ANO-PAGTO-PARC     PIC 9(004).
                07 SLIG73-VND-MES-PAGTO-PARC     PIC 9(002).
                07 SLIG73-VND-DIA-PAGTO-PARC     PIC 9(002).
             05 SLIG73-VND-CPTO-SIT-PARC         PIC 9(003) COMP-3.
                88 SLIG73-VND-CTPO-PAGTO-NORMAL  VALUE 1.
                88 SLIG73-VND-CTPO-PAGTO-ANTECI  VALUE 2.
                88 SLIG73-VND-CTPO-BAIXA-NORMAL  VALUE 3.
                88 SLIG73-VND-CTPO-BAIXA-ANTECI  VALUE 4.
                88 SLIG73-VND-CTPO-BAIXA-EXCLUS  VALUE 5.
                88 SLIG73-VND-CTPO-SIT-PARC-VENC VALUE 6.
                88 SLIG73-VND-CTPO-SIT-PARC-FUTU VALUE 7.
             05 SLIG73-VND-DT-ENTRADA-ORIGEM     PIC 9(008).
             05 SLIG73-VND-DT-ENTRADA-ORIGEM-R   REDEFINES
                SLIG73-VND-DT-ENTRADA-ORIGEM.
                07 SLIG73-VND-ANO-ENTRADA-ORIGEM PIC 9(004).
                07 SLIG73-VND-MES-ENTRADA-ORIGEM PIC 9(002).
                07 SLIG73-VND-DIA-ENTRADA-ORIGEM PIC 9(002).
             05 SLIG73-VND-VL-PRINC-PARC         PIC 9(013)V99 COMP-3.
             05 SLIG73-VND-IND-PAGTO             PIC X(001).
                88 SLIG73-VND-IND-PAGTO-COMP     VALUE '1'.
                88 SLIG73-VND-IND-PAGTO-FORN     VALUE '2'.
             05 SLIG73-VND-BCO-DIR-REGIONAL      PIC 9(003) COMP-3.
             05 SLIG73-VND-DIR-REGIONAL          PIC 9(005) COMP-3.
             05 SLIG73-VND-BCO-GERC-REGIONAL     PIC 9(003) COMP-3.
             05 SLIG73-VND-GERC-REGIONAL         PIC 9(005) COMP-3.
             05 SLIG73-VND-GRP-ECONOMICO-COMP    PIC 9(009) COMP-3.
             05 SLIG73-VND-GRP-ECONOMICO-FORN    PIC 9(009) COMP-3.
             05 SLIG73-VND-CGC-CPF-COMP-ST       PIC X(009).
             05 SLIG73-VND-FILIAL-COMP-ST        PIC X(004).
             05 SLIG73-VND-CONTROLE-COMP-ST      PIC 9(002) COMP-3.
             05 SLIG73-VND-CGC-CPF-FORN-ST       PIC X(009).
             05 SLIG73-VND-FILIAL-FORN-ST        PIC X(004).
             05 SLIG73-VND-CONTROLE-FORN-ST      PIC 9(002) COMP-3.
             05 FILLER                           PIC X(002).
          03 SLIG73-REG-CHEQUES REDEFINES SLIG73-REG-PRODUTOS.
             05 SLIG73-CHQ-CGC-CPF-EMITT         PIC 9(009) COMP-3.
             05 SLIG73-CHQ-FILIAL-EMITT          PIC 9(004) COMP-3.
             05 SLIG73-CHQ-CONTROLE-EMITT        PIC 9(002) COMP-3.
             05 SLIG73-CHQ-TIPO-PESSOA-EMITT     PIC X(001).
             05 SLIG73-CHQ-BCO-EMITT             PIC 9(003) COMP-3.
             05 SLIG73-CHQ-AGENCIA-EMITT         PIC 9(005) COMP-3.
             05 SLIG73-CHQ-CONTA-EMITT           PIC 9(013) COMP-3.
             05 SLIG73-CHQ-NUM-CHEQUE            PIC 9(007) COMP-3.
             05 SLIG73-CHQ-DIG-CHEQUE            PIC X(001).
             05 SLIG73-CHQ-NUM-LOTE              PIC 9(007) COMP-3.
             05 SLIG73-CHQ-CARTEIRA              PIC 9(003) COMP-3.
                88 SLIG73-CHQ-CARTEIRA-SIMPLES    VALUE 1.
                88 SLIG73-CHQ-CARTEIRA-CAUCIONADA VALUE 2.
                88 SLIG73-CHQ-CARTEIRA-DESCONTO   VALUE 3.
             05 SLIG73-CHQ-NOME-EMITT            PIC X(040).
             05 SLIG73-CHQ-BCO-CLI               PIC 9(003) COMP-3.
             05 SLIG73-CHQ-AGENCIA-CLI           PIC 9(005) COMP-3.
             05 SLIG73-CHQ-CONTA-CLI             PIC 9(013) COMP-3.
             05 SLIG73-CHQ-CGC-CPF-CLI           PIC 9(009) COMP-3.
             05 SLIG73-CHQ-FILIAL-CLI            PIC 9(004) COMP-3.
             05 SLIG73-CHQ-CONTROLE-CLI          PIC 9(002) COMP-3.
             05 SLIG73-CHQ-TIPO-PESSOA-CLI       PIC X(001).
             05 SLIG73-CHQ-NOME-CLI              PIC X(040).
             05 SLIG73-CHQ-DT-DEPOSITO           PIC 9(008).
             05 SLIG73-CHQ-DT-DEPOSITO-RED REDEFINES
                SLIG73-CHQ-DT-DEPOSITO.
                10 SLIG73-CHQ-ANO-DEPOSITO       PIC 9(004).
                10 SLIG73-CHQ-MES-DEPOSITO       PIC 9(002).
                10 SLIG73-CHQ-DIA-DEPOSITO       PIC 9(002).
             05 SLIG73-CHQ-DT-CONTABIL           PIC 9(008).
             05 SLIG73-CHQ-DT-CONTABIL-RED REDEFINES
                SLIG73-CHQ-DT-CONTABIL.
                10 SLIG73-CHQ-ANO-CONTABIL       PIC 9(004).
                10 SLIG73-CHQ-MES-CONTABIL       PIC 9(002).
                10 SLIG73-CHQ-DIA-CONTABIL       PIC 9(002).
             05 SLIG73-CHQ-DT-ENTRADA-ORIGEM     PIC 9(008).
             05 SLIG73-CHQ-DT-ENTRADA-ORIGEM-R   REDEFINES
                SLIG73-CHQ-DT-ENTRADA-ORIGEM.
                10 SLIG73-CHQ-ANO-ENTRADA-ORIGEM PIC 9(004).
                10 SLIG73-CHQ-MES-ENTRADA-ORIGEM PIC 9(002).
                10 SLIG73-CHQ-DIA-ENTRADA-ORIGEM PIC 9(002).
             05 SLIG73-CHQ-DT-SITUACAO           PIC 9(008).
             05 SLIG73-CHQ-DT-SITUACAO-RED REDEFINES
                SLIG73-CHQ-DT-SITUACAO.
                10 SLIG73-CHQ-ANO-SITUACAO       PIC 9(004).
                10 SLIG73-CHQ-MES-SITUACAO       PIC 9(002).
                10 SLIG73-CHQ-DIA-SITUACAO       PIC 9(002).
             05 SLIG73-CHQ-CTPO-CHEQ             PIC 9(003) COMP-3.
                88 SLIG73-CHQ-CTPO-CHEQ-NORML    VALUE 1.
                88 SLIG73-CHQ-CTPO-CHEQ-BAIXA    VALUE 2.
                88 SLIG73-CHQ-CTPO-CHEQ-COMPE    VALUE 3.
                88 SLIG73-CHQ-CTPO-CHEQ-DEVOL    VALUE 4.
                88 SLIG73-CHQ-CTPO-CHEQ-REAP-PG  VALUE 5.
                88 SLIG73-CHQ-CTPO-CHEQ-REAP-DEV VALUE 6.
             05 SLIG73-CHQ-VL-PRINC-CHEQ         PIC 9(013)V99 COMP-3.
             05 SLIG73-CHQ-BCO-DIR-REGIONAL      PIC 9(003) COMP-3.
             05 SLIG73-CHQ-DIR-REGIONAL          PIC 9(005) COMP-3.
             05 SLIG73-CHQ-BCO-GERC-REGIONAL     PIC 9(003) COMP-3.
             05 SLIG73-CHQ-GERC-REGIONAL         PIC 9(005) COMP-3.
             05 SLIG73-CHQ-GRP-ECONOMICO-EMITT   PIC 9(009) COMP-3.
             05 SLIG73-CHQ-GRP-ECONOMICO-CLI     PIC 9(009) COMP-3.
             05 SLIG73-CHQ-CGC-CPF-EMITT-ST      PIC X(009).
             05 SLIG73-CHQ-FILIAL-EMITT-ST       PIC X(004).
             05 SLIG73-CHQ-CONTROLE-EMITT-ST     PIC 9(002) COMP-3.
             05 SLIG73-CHQ-CGC-CPF-CLI-ST        PIC X(009).
             05 SLIG73-CHQ-FILIAL-CLI-ST         PIC X(004).
             05 SLIG73-CHQ-CONTROLE-CLI-ST       PIC 9(002) COMP-3.
             05 FILLER                           PIC X(007).
