      ***===========================================================***
      *** NOME INC                                     LENGTH=0308  ***
      *** I#SLIG6B                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO REGS.SELECIONADOS TITULOS/CHEQUES/VENDOR**
      **             PARA EMISSAO RELATORIO ANALISE PERFOMANCE       **
      ***===========================================================***
       01 REG-SELECIONADO.
          03 SLIG6B-CTRL.
             05 SLIG6B-CGC-CPF-CTRL-ST.
                07 SLIG6B-CGC-CPF-CTRL           PIC 9(009).
             05 SLIG6B-PRODUTO-CTRL              PIC X(001).
             05 SLIG6B-CARTEIRA-CTRL             PIC 9(001).
             05 SLIG6B-CTRL-TIT-VND.
                07 SLIG6B-CD-PRODUTO-CTRL        PIC 9(002).
                07 SLIG6B-CD-CNEGOC-CTRL         PIC 9(011).
                07 SLIG6B-CD-NOSNRO-CTRL         PIC 9(011).
             05 SLIG6B-CTRL-CHQ REDEFINES SLIG6B-CTRL-TIT-VND.
                07 SLIG6B-AGENCIA-EMITT-CTRL     PIC 9(004).
                07 SLIG6B-CONTA-EMITT-CTRL       PIC 9(013).
                07 SLIG6B-NUM-CHEQUE-CTRL        PIC 9(007).
             05 SLIG6B-DT-ENTRADA-CTRL           PIC 9(008).
             05 SLIG6B-DT-VENCTO-CTRL            PIC 9(008).
          03 SLIG6B-IDENT-DOCTO                  PIC X(035).
          03 SLIG6B-IDENT-DOCTO-TIT REDEFINES SLIG6B-IDENT-DOCTO.
             05  SLIG6B-CD-PRODUTO-CLI           PIC 9(002).
             05  FILLER                          PIC X(002).
             05  SLIG6B-AGENCIA-CLI              PIC 9(004).
             05  FILLER                          PIC X(002).
             05  SLIG6B-CONTA-CLI                PIC 9(007).
             05  FILLER                          PIC X(002).
             05  SLIG6B-NUM-TIT                  PIC 9(011).
             05  FILLER                          PIC X(005).
          03 SLIG6B-IDENT-DOCTO-VND REDEFINES SLIG6B-IDENT-DOCTO.
             05  SLIG6B-CPRODT-CLI               PIC 9(003).
             05  FILLER                          PIC X(001).
             05  SLIG6B-CFAML-CLI                PIC 9(001).
             05  FILLER                          PIC X(001).
             05  SLIG6B-CCONTR-CLI               PIC 9(009).
             05  FILLER                          PIC X(001).
             05  SLIG6B-CSEQ-CLI                 PIC 9(003).
             05  FILLER                          PIC X(001).
             05  SLIG6B-CPARCELA-CLI             PIC 9(003).
             05  FILLER                          PIC X(001).
             05  SLIG6B-NUM-DOCTO                PIC 9(011).
          03 SLIG6B-IDENT-DOCTO-CHQ REDEFINES SLIG6B-IDENT-DOCTO.
             05  SLIG6B-BANCO-EMITT              PIC 9(003).
             05  FILLER                          PIC X(002).
             05  SLIG6B-AGEN-EMITT               PIC 9(004).
             05  FILLER                          PIC X(001).
             05  SLIG6B-CTA-EMITT                PIC 9(013).
             05  FILLER                          PIC X(001).
             05  SLIG6B-NUM-CHEQUE-EMITT         PIC 9(007).
             05  FILLER                          PIC X(004).
          03 SLIG6B-PERIODOS.
             05  SLIG6B-DT-PER-INI               PIC 9(008).
             05  SLIG6B-DT-PER-FIM               PIC 9(008).
          03 SLIG6B-REG-PRODUTOS                 PIC X(236).
          03 SLIG6B-REG-TITULOS REDEFINES SLIG6B-REG-PRODUTOS.
             05 SLIG6B-TIT-BCO-CLIENTE           PIC 9(003) COMP-3.
             05 SLIG6B-TIT-AGENCIA-CLIENTE       PIC 9(005) COMP-3.
             05 SLIG6B-TIT-CONTA-CLIENTE         PIC 9(013) COMP-3.
             05 SLIG6B-TIT-CARTEIRA              PIC 9(003) COMP-3.
             05 SLIG6B-TIT-CCLI-COBR             PIC 9(009) COMP-3.
             05 SLIG6B-TIT-CGC-CPF-CLIENTE       PIC 9(009) COMP-3.
             05 SLIG6B-TIT-FILIAL-CLIENTE        PIC 9(005) COMP-3.
             05 SLIG6B-TIT-CONTROLE-CLIENTE      PIC 9(002).
             05 SLIG6B-TIT-BCO-SACADO            PIC 9(003) COMP-3.
             05 SLIG6B-TIT-AGENCIA-SACADO        PIC 9(005) COMP-3.
             05 SLIG6B-TIT-CONTA-SACADO          PIC 9(013) COMP-3.
             05 SLIG6B-TIT-CGC-CPF-SACADO        PIC 9(009) COMP-3.
             05 SLIG6B-TIT-FILIAL-SACADO         PIC 9(005) COMP-3.
             05 SLIG6B-TIT-CONTROLE-SACADO       PIC 9(002).
             05 SLIG6B-TIT-VL-PRINC-PROD         PIC 9(017) COMP-3.
             05 SLIG6B-TIT-VL-PAGO-PROD          PIC 9(015) COMP-3.
             05 SLIG6B-TIT-VL-DESC-ABAT-PROD     PIC 9(015) COMP-3.
             05 SLIG6B-TIT-DT-VENCTO-PROD        PIC 9(009) COMP-3.
             05 SLIG6B-TIT-DT-PAGTO-PROD         PIC 9(009) COMP-3.
             05 SLIG6B-TIT-DT-ENTRADA-ORIGEM     PIC 9(009) COMP-3.
             05 SLIG6B-TIT-DT-ENTRADA-SISTEMA    PIC 9(009) COMP-3.
             05 SLIG6B-TIT-DT-VENCTO-ANTERIOR    PIC 9(009) COMP-3.
             05 SLIG6B-TIT-CD-PRODUTO            PIC 9(002).
             05 SLIG6B-TIT-CD-CNEGOC             PIC 9(011) COMP-3.
             05 SLIG6B-TIT-CD-NOSNRO             PIC 9(011) COMP-3.
             05 SLIG6B-TIT-CD-SEQUENCIA          PIC 9(003) COMP-3.
             05 SLIG6B-TIT-CD-COD-NTAX           PIC 9(005) COMP-3.
             05 SLIG6B-TIT-CD-OCO-REM-L          PIC 9(003) COMP-3.
             05 SLIG6B-TIT-CD-MOT-REM-SIS        PIC 9(003) COMP-3.
             05 SLIG6B-TIT-CD-ESPECIE            PIC 9(003) COMP-3.
             05 SLIG6B-TIT-CD-ORIGEM             PIC 9(003) COMP-3.
             05 SLIG6B-TIT-DT-INICIO-PER         PIC 9(009) COMP-3.
             05 SLIG6B-TIT-DT-FINAL-PER          PIC 9(009) COMP-3.
             05 SLIG6B-TIT-CGC-CPF-CLIENTE-ST    PIC X(009).
             05 SLIG6B-TIT-FILIAL-CLIENTE-ST     PIC X(005).
             05 SLIG6B-TIT-CONTROLE-CLIENTE-ST   PIC 9(002).
             05 SLIG6B-TIT-CGC-CPF-SACADO-ST     PIC X(009).
             05 SLIG6B-TIT-FILIAL-SACADO-ST      PIC X(005).
             05 SLIG6B-TIT-CONTROLE-SACADO-ST    PIC 9(002).
             05  FILLER                          PIC X(066).
          03 SLIG6B-REG-VENDOR REDEFINES SLIG6B-REG-PRODUTOS.
             05 SLIG6B-VND-CGC-CPF-COMP          PIC 9(009) COMP-3.
             05 SLIG6B-VND-FILIAL-COMP           PIC 9(004) COMP-3.
             05 SLIG6B-VND-CONTROLE-COMP         PIC 9(002) COMP-3.
             05 SLIG6B-VND-TIPO-PESSOA-COMP      PIC X(001).
             05 SLIG6B-VND-BCO-COMP              PIC 9(003) COMP-3.
             05 SLIG6B-VND-AGENCIA-COMP          PIC 9(005) COMP-3.
             05 SLIG6B-VND-CONTA-COMP            PIC 9(013) COMP-3.
             05 SLIG6B-VND-NOME-COMP             PIC X(040).
             05 SLIG6B-VND-CPRODT                PIC 9(003) COMP-3.
             05 SLIG6B-VND-CFAML                 PIC 9(001) COMP-3.
             05 SLIG6B-VND-CCONTR                PIC 9(009) COMP-3.
             05 SLIG6B-VND-CSEQ                  PIC 9(003) COMP-3.
             05 SLIG6B-VND-CPARCELA              PIC 9(003) COMP-3.
             05 SLIG6B-VND-NUM-PAGTO             PIC 9(009) COMP-3.
             05 SLIG6B-VND-NOSSO-NUMERO          PIC 9(011) COMP-3.
             05 SLIG6B-VND-CGC-CPF-FORN          PIC 9(009) COMP-3.
             05 SLIG6B-VND-FILIAL-FORN           PIC 9(004) COMP-3.
             05 SLIG6B-VND-CONTROLE-FORN         PIC 9(002) COMP-3.
             05 SLIG6B-VND-TIPO-PESSOA-FORN      PIC X(001).
             05 SLIG6B-VND-BCO-FORN              PIC 9(003) COMP-3.
             05 SLIG6B-VND-AGENCIA-FORN          PIC 9(005) COMP-3.
             05 SLIG6B-VND-CONTA-FORN            PIC 9(013) COMP-3.
             05 SLIG6B-VND-NOME-FORN             PIC X(040).
             05 SLIG6B-VND-DT-VENCTO-PARC        PIC 9(008).
             05 SLIG6B-VND-DT-VENCTO-PARC-RED REDEFINES
                SLIG6B-VND-DT-VENCTO-PARC.
                07 SLIG6B-VND-ANO-VENCTO-PARC    PIC 9(004).
                07 SLIG6B-VND-MES-VENCTO-PARC    PIC 9(002).
                07 SLIG6B-VND-DIA-VENCTO-PARC    PIC 9(002).
             05 SLIG6B-VND-DT-PAGTO-PARC         PIC 9(008).
             05 SLIG6B-VND-DT-PAGTO-PARC-RED REDEFINES
                SLIG6B-VND-DT-PAGTO-PARC.
                07 SLIG6B-VND-ANO-PAGTO-PARC     PIC 9(004).
                07 SLIG6B-VND-MES-PAGTO-PARC     PIC 9(002).
                07 SLIG6B-VND-DIA-PAGTO-PARC     PIC 9(002).
             05 SLIG6B-VND-CPTO-SIT-PARC         PIC 9(003) COMP-3.
                88 SLIG6B-VND-CTPO-PAGTO-NORMAL  VALUE 1.
                88 SLIG6B-VND-CTPO-PAGTO-ANTECI  VALUE 2.
                88 SLIG6B-VND-CTPO-BAIXA-NORMAL  VALUE 3.
                88 SLIG6B-VND-CTPO-BAIXA-ANTECI  VALUE 4.
                88 SLIG6B-VND-CTPO-BAIXA-EXCLUS  VALUE 5.
                88 SLIG6B-VND-CTPO-SIT-PARC-VENC VALUE 6.
                88 SLIG6B-VND-CTPO-SIT-PARC-FUTU VALUE 7.
             05 SLIG6B-VND-DT-ENTRADA-ORIGEM     PIC 9(008).
             05 SLIG6B-VND-DT-ENTRADA-ORIGEM-R   REDEFINES
                SLIG6B-VND-DT-ENTRADA-ORIGEM.
                07 SLIG6B-VND-ANO-ENTRADA-ORIGEM PIC 9(004).
                07 SLIG6B-VND-MES-ENTRADA-ORIGEM PIC 9(002).
                07 SLIG6B-VND-DIA-ENTRADA-ORIGEM PIC 9(002).
             05 SLIG6B-VND-VL-PRINC-PARC         PIC 9(013)V99 COMP-3.
             05 SLIG6B-VND-IND-PAGTO             PIC X(001).
                88 SLIG6B-VND-IND-PAGTO-COMP     VALUE '1'.
                88 SLIG6B-VND-IND-PAGTO-FORN     VALUE '2'.
             05 SLIG6B-VND-BCO-DIR-REGIONAL      PIC 9(003) COMP-3.
             05 SLIG6B-VND-DIR-REGIONAL          PIC 9(005) COMP-3.
             05 SLIG6B-VND-BCO-GERC-REGIONAL     PIC 9(003) COMP-3.
             05 SLIG6B-VND-GERC-REGIONAL         PIC 9(005) COMP-3.
             05 SLIG6B-VND-GRP-ECONOMICO-COMP    PIC 9(009) COMP-3.
             05 SLIG6B-VND-GRP-ECONOMICO-FORN    PIC 9(009) COMP-3.
             05 SLIG6B-VND-CGC-CPF-COMP-ST       PIC X(009).
             05 SLIG6B-VND-FILIAL-COMP-ST        PIC X(004).
             05 SLIG6B-VND-CONTROLE-COMP-ST      PIC 9(002) COMP-3.
             05 SLIG6B-VND-CGC-CPF-FORN-ST       PIC X(009).
             05 SLIG6B-VND-FILIAL-FORN-ST        PIC X(004).
             05 SLIG6B-VND-CONTROLE-FORN-ST      PIC 9(002) COMP-3.
             05 FILLER                           PIC X(002).
          03 SLIG6B-REG-CHEQUES REDEFINES SLIG6B-REG-PRODUTOS.
             05 SLIG6B-CHQ-CGC-CPF-EMITT         PIC 9(009) COMP-3.
             05 SLIG6B-CHQ-FILIAL-EMITT          PIC 9(004) COMP-3.
             05 SLIG6B-CHQ-CONTROLE-EMITT        PIC 9(002) COMP-3.
             05 SLIG6B-CHQ-TIPO-PESSOA-EMITT     PIC X(001).
             05 SLIG6B-CHQ-BCO-EMITT             PIC 9(003) COMP-3.
             05 SLIG6B-CHQ-AGENCIA-EMITT         PIC 9(005) COMP-3.
             05 SLIG6B-CHQ-CONTA-EMITT           PIC 9(013) COMP-3.
             05 SLIG6B-CHQ-NUM-CHEQUE            PIC 9(007) COMP-3.
             05 SLIG6B-CHQ-DIG-CHEQUE            PIC X(001).
             05 SLIG6B-CHQ-NUM-LOTE              PIC 9(007) COMP-3.
             05 SLIG6B-CHQ-CARTEIRA              PIC 9(003) COMP-3.
                88 SLIG6B-CHQ-CARTEIRA-SIMPLES    VALUE 1.
                88 SLIG6B-CHQ-CARTEIRA-CAUCIONADA VALUE 2.
                88 SLIG6B-CHQ-CARTEIRA-DESCONTO   VALUE 3.
             05 SLIG6B-CHQ-NOME-EMITT            PIC X(040).
             05 SLIG6B-CHQ-BCO-CLI               PIC 9(003) COMP-3.
             05 SLIG6B-CHQ-AGENCIA-CLI           PIC 9(005) COMP-3.
             05 SLIG6B-CHQ-CONTA-CLI             PIC 9(013) COMP-3.
             05 SLIG6B-CHQ-CGC-CPF-CLI           PIC 9(009) COMP-3.
             05 SLIG6B-CHQ-FILIAL-CLI            PIC 9(004) COMP-3.
             05 SLIG6B-CHQ-CONTROLE-CLI          PIC 9(002) COMP-3.
             05 SLIG6B-CHQ-TIPO-PESSOA-CLI       PIC X(001).
             05 SLIG6B-CHQ-NOME-CLI              PIC X(040).
             05 SLIG6B-CHQ-DT-DEPOSITO           PIC 9(008).
             05 SLIG6B-CHQ-DT-DEPOSITO-RED REDEFINES
                SLIG6B-CHQ-DT-DEPOSITO.
                10 SLIG6B-CHQ-ANO-DEPOSITO       PIC 9(004).
                10 SLIG6B-CHQ-MES-DEPOSITO       PIC 9(002).
                10 SLIG6B-CHQ-DIA-DEPOSITO       PIC 9(002).
             05 SLIG6B-CHQ-DT-CONTABIL           PIC 9(008).
             05 SLIG6B-CHQ-DT-CONTABIL-RED REDEFINES
                SLIG6B-CHQ-DT-CONTABIL.
                10 SLIG6B-CHQ-ANO-CONTABIL       PIC 9(004).
                10 SLIG6B-CHQ-MES-CONTABIL       PIC 9(002).
                10 SLIG6B-CHQ-DIA-CONTABIL       PIC 9(002).
             05 SLIG6B-CHQ-DT-ENTRADA-ORIGEM     PIC 9(008).
             05 SLIG6B-CHQ-DT-ENTRADA-ORIGEM-R   REDEFINES
                SLIG6B-CHQ-DT-ENTRADA-ORIGEM.
                10 SLIG6B-CHQ-ANO-ENTRADA-ORIGEM PIC 9(004).
                10 SLIG6B-CHQ-MES-ENTRADA-ORIGEM PIC 9(002).
                10 SLIG6B-CHQ-DIA-ENTRADA-ORIGEM PIC 9(002).
             05 SLIG6B-CHQ-DT-SITUACAO           PIC 9(008).
             05 SLIG6B-CHQ-DT-SITUACAO-RED REDEFINES
                SLIG6B-CHQ-DT-SITUACAO.
                10 SLIG6B-CHQ-ANO-SITUACAO       PIC 9(004).
                10 SLIG6B-CHQ-MES-SITUACAO       PIC 9(002).
                10 SLIG6B-CHQ-DIA-SITUACAO       PIC 9(002).
             05 SLIG6B-CHQ-CTPO-CHEQ             PIC 9(003) COMP-3.
                88 SLIG6B-CHQ-CTPO-CHEQ-NORML    VALUE 1.
                88 SLIG6B-CHQ-CTPO-CHEQ-BAIXA    VALUE 2.
                88 SLIG6B-CHQ-CTPO-CHEQ-COMPE    VALUE 3.
                88 SLIG6B-CHQ-CTPO-CHEQ-DEVOL    VALUE 4.
                88 SLIG6B-CHQ-CTPO-CHEQ-REAP-PG  VALUE 5.
                88 SLIG6B-CHQ-CTPO-CHEQ-REAP-DEV VALUE 6.
             05 SLIG6B-CHQ-VL-PRINC-CHEQ         PIC 9(013)V99 COMP-3.
             05 SLIG6B-CHQ-BCO-DIR-REGIONAL      PIC 9(003) COMP-3.
             05 SLIG6B-CHQ-DIR-REGIONAL          PIC 9(005) COMP-3.
             05 SLIG6B-CHQ-BCO-GERC-REGIONAL     PIC 9(003) COMP-3.
             05 SLIG6B-CHQ-GERC-REGIONAL         PIC 9(005) COMP-3.
             05 SLIG6B-CHQ-GRP-ECONOMICO-EMITT   PIC 9(009) COMP-3.
             05 SLIG6B-CHQ-GRP-ECONOMICO-CLI     PIC 9(009) COMP-3.
             05 SLIG6B-CODI-DEVOL-CHQ            PIC 9(002).
             05 SLIG6B-CHQ-CGC-CPF-EMITT-ST      PIC X(009).
             05 SLIG6B-CHQ-FILIAL-EMITT-ST       PIC X(004).
             05 SLIG6B-CHQ-CONTROLE-EMITT-ST     PIC 9(002) COMP-3.
             05 SLIG6B-CHQ-CGC-CPF-CLI-ST        PIC X(009).
             05 SLIG6B-CHQ-FILIAL-CLI-ST         PIC X(004).
             05 SLIG6B-CHQ-CONTROLE-CLI-ST       PIC 9(002) COMP-3.
             05 FILLER                           PIC X(005).
