      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG6216.
       AUTHOR.     FABIO AUGUSTO FINK.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG6216                                    *
      *    ANALISTA....:   SOLANGE R. F. BARBEIRO    - DTS / GP. 75    *
      *    DATA........:   AGOSTO/2004                                 *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   OBETM DADOS DO CLIENTE (NOME,DIRETORIA,     *
      *                    GERENCIA,GRUPO E CPF )                      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                    MVTCHQSR                    I#SLIG28        *
      *                    CADCLIE                     I#DEVEN7        *
      *                    EMITCHEQ                    I#SLIG28        *
      *                    SRELCTRL                       -            *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GRUP34 - AREA PARA ACESSO AO MODULO GRUP0930              *
      *    I#MESUM4 - AREA PARA ACESSO AO MODULO MESU9014              *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    MESU9014 - OBTER DIRETORIA REGIONAL                         *
      *    MESU9023 - OBTER GERENCIA REGIONAL                          *
      *    GRUP0930 - CONSULTA DE EMPRESAS                             *
      *    BRAD0300 - LEITURA ARQUIVO COMPRIMIDO                       *
      *    POOL7100 - TRATAMENTO DE ERROS                              *
           EJECT
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT MVTCHQSR ASSIGN      TO UT-S-MVTCHQSR
           FILE STATUS                 IS WRK-FS-MVTCHQSR.


           SELECT EMITCHEQ ASSIGN      TO UT-S-EMITCHEQ
           FILE STATUS                 IS WRK-FS-EMITCHEQ.

           SELECT SRELCTRL ASSIGN      TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE MOVIMENTO DIARIO E DECENAL DE CHEQUE     *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  MVTCHQSR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG28

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE MOVIMENTO DIARIO E DECENAL DE CHEQUE     *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  EMITCHEQ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-CHEQUE-SAIDA               PIC  X(300).

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE CONTROLE OPERACIONAL                   *
      *            ORG. SEQUENCIAL     -    LRECL   =   081            *
      *----------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SRELCTRL                PIC  X(081).


      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6216 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-MVTCHQSR          PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-EMITCHEQ          PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CADCLIE           PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-N-ENC-CLIB2000          PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-ERROR-MESU9014          PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-ERROR-MESU9023          PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-NOME-CPF-ATLZ       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-CPF-ZEROS-OUT       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-CPF-N-ZEROS-OUT     PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-NOME-N-ENC-OUT      PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-NOME-SPACES-OUT     PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-NOME-OK-OUT         PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-GRP-ZEROS-OUT       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-GRP-N-ZEROS-OUT     PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-DIR-ZEROS-OUT       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-DIR-N-ZEROS-OUT     PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-ACESSOS-MESU9014        PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-MESU9014-DIR-N-ENC      PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-MESU9014-DIR-ENC        PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-ACESSOS-MESU9023        PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-MESU9023-GER-ENC        PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-ACESSOS-GRUP0930        PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRUP0930-GRP-ENC        PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRUP0930-GRP-N-ENC      PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRUP0930-ERRO           PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-MESU9023-GER-N-ENC      PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-GER-ZEROS-OUT       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-CLI-GER-N-ZEROS-OUT     PIC  9(015) COMP-3  VALUE ZEROS.
       77  WRK-FLAG                    PIC  9(001)         VALUE ZEROS.
       77  WRK-GRUP0930-MSG-ERRO       PIC  X(035)         VALUE ZEROS.
       77  WRK-MESU9023-MSG-ERRO       PIC  X(035)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-MVTCHQSR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-EMITCHEQ             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADCLIE              PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

           EJECT
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-CADCLIE                 PIC  X(008)         VALUE
           'CADCLIE'.

       77  WRK-CGC-CPF-CLI             PIC  X(009)         VALUE SPACES.
       77  WRK-FILIAL-CLI              PIC  X(004)         VALUE SPACES.
       77  WRK-CONTROLE-CLI            PIC  9(002) COMP-3  VALUE ZEROS.
       77  WRK-BCO-DIR-REGIONAL        PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-DIR-REGIONAL            PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-BCO-GERC-REGIONAL       PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-GERC-REGIONAL           PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-GRP-ECONOMICO-CLI       PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-TIPO-PESSOA-CLI         PIC  X(001)         VALUE SPACES.
       77  WRK-NOME-CLI                PIC  X(040)         VALUE SPACES.

       01  WRK-IO-PCB                  PIC  X(032)         VALUE SPACES.
       01  WRK-ALT-PCB                 PIC  X(032)         VALUE SPACES.
       01  WRK-INDC-COUNT              PIC  9(002)         VALUE ZEROS.


       01  WRK-CHV-DEVE17.
           03  WRK-BANCO-DEVE          PIC  9(003)         VALUE ZEROS.
           03  WRK-AGENCIA-DEVE        PIC  9(005)         VALUE ZEROS.
           03  WRK-CONTA-DEVE          PIC  9(013)         VALUE ZEROS.


       01  WRK-CHV-ATU.
           03  WRK-BCO-ATU             PIC  9(003)         VALUE ZEROS.
           03  WRK-AGE-ATU             PIC  9(005)         VALUE ZEROS.
           03  WRK-CTA-ATU             PIC  9(013)         VALUE ZEROS.

       01  WRK-CHV-ANT.
           03  WRK-BCO-ANT             PIC  9(003)         VALUE ZEROS.
           03  WRK-AGE-ANT             PIC  9(005)         VALUE ZEROS.
           03  WRK-CTA-ANT             PIC  9(013)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG001              PIC  X(075)         VALUE
               '** ARQUIVO MVTCHQSR VAZIO **'.
           03  WRK-MSG002              PIC  X(075)         VALUE
               '** CLIENTE NAO ENCONTRADO NO CLIB **'.
           03  WRK-MSG003.
               05  FILLER              PIC  X(028)         VALUE
                   '** ERRO NO ACESSO AO MODULO '.
               05  WRK-NM-MODULO       PIC  X(008)         VALUE SPACES.
               05  FILLER              PIC  X(017)         VALUE
                   ' - RETURN CODE = '.
               05  WRK-RET-COD         PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(003)         VALUE ' **'.
           03  WRK-MSG-FORA-SEQ        PIC  X(075)         VALUE
               '** ARQUIVO MVTCHQSR FORA DE SEQUENCIA **'.

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-P7600-ANO       PIC  9(004)         VALUE ZEROS.
               05  WRK-P7600-MES       PIC  9(002)         VALUE ZEROS.
               05  WRK-P7600-DIA       PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(012)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** INCIO DA INC I#DEVEN7 DO CADACLIE   ***'.
      *----------------------------------------------------------------*

       COPY I#DEVEN7.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO MESU9023 ***'.
      *----------------------------------------------------------------*

       01  WRK-9023-AMBIENTE            PIC  X(001)        VALUE SPACES.
       01  WRK-9023-ERRO-AREA           PIC  X(107)        VALUE SPACES.

       01  WRK-9023-PARM.
           03  WRK-9023-ENTRADA.
               05  WRK-9023-NOME-PGM    PIC  X(008)        VALUE SPACES.
               05  WRK-9023-COD-RET     PIC  9(002)        VALUE ZEROS.
               05  WRK-9023-TPO-RELAC   PIC  9(003)        VALUE ZEROS.
               05  WRK-9023-BANCO       PIC  9(005)        VALUE ZEROS.
               05  WRK-9023-AGENCIA     PIC  9(005)        VALUE ZEROS.
           03  WRK-9023-SAIDA.
               05  WRK-9023-EMPR-RELAC  PIC  9(005)        VALUE ZEROS.
               05  WRK-9023-DEPTO-RELAC PIC  9(005)        VALUE ZEROS.
               05  WRK-9023-NOME-DEPDC  PIC  X(050)        VALUE SPACES.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DEFINICAO DO RELATORIO - CABECALHOS ***'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(023)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(047)         VALUE
               'SLIG - SISTEMA DE LIQUIDEZ GERAL'.
           03  FILLER                  PIC  X(010)         VALUE
               'FOL. UNICA'.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(070)         VALUE
               'SLIG6216'.
           03  CB2-DIA                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MES                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-ANO                 PIC  9(004)         VALUE ZEROS.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(043)         VALUE
               'OBTEM DADOS DO CLIENTE'.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(029)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'CONTROLE OPERACIONAL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DEFINICAO DO RELATORIO - DETALHES ***'.
      *----------------------------------------------------------------*

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS LIDOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(MVTCHQSR) : '.
           03  LT1-TOT-LIDOS           PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT2.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS GRAVADOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(EMITCHEQ) : '.
           03  LT2-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT3.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGS COM ERROS    '.
           03  FILLER                  PIC  X(013)         VALUE
               '(MESU9014) : '.
           03  LT3-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT4.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGS COM ERROS    '.
           03  FILLER                  PIC  X(013)         VALUE
               '(MESU9023) : '.
           03  LT4-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT5.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGS LIDOS        '.
           03  FILLER                  PIC  X(013)         VALUE
               '(CADCLIE)  : '.
           03  LT5-TOT-LIDOS           PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT6.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGS NAO ENCONT. '.
           03  FILLER                  PIC  X(013)         VALUE
               '(CADCLIE)  : '.
           03  LT6-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT7.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT CLIENTE NOME/CNPJ ATUALZ   : '.
           03  LT7-TOT-CLIE-ATUALZ     PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT8.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT CLIENTE CNPJ ZERADOS SAIDA : '.
           03  LT8-TOT-CNPJ-ZEROS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT9.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DIFERENTES DE ZEROS SAIDA  : '.
           03  LT9-TOT-CNPJ-DIF-ZEROS  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT10.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT CLIENTE NOME NAO ENC SAIDA : '.
           03  LT10-TOT-NOME-N-ENC     PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT11.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT CLIENTE NOME SPACES SAIDA  : '.
           03  LT11-TOT-NOME-SPACES    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT12.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT CLIENTE NOME OK SAIDA      : '.
           03  LT12-TOT-NOME-OK        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT13.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT ACESSOS AO GRUP0930        : '.
           03  LT13-TOT-ACESSO-GRUP0930 PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT14.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DE ERROS NO GRUP0930       : '.
           03  LT14-TOT-ERROS-GRUP0930 PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT15.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DE ENCONTRADOS NO GRUP0930 : '.
           03  LT15-TOT-ENC-GRUP0930   PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT16.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DE NAO ENCONTR NO GRUP0930 : '.
           03  LT16-TOT-N-ENC-GRUP0930 PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT17.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DE GRUPOS ZERADOS NA SAIDA : '.
           03  LT17-TOT-GRP-ZEROS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT18.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DIFERENTES DE ZEROS SAIDA  : '.
           03  LT18-TOT-GRP-DIF-ZEROS  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT19.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT ACESSOS AO MESU9014        : '.
           03  LT19-TOT-ACESSO-MESU9014 PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT20.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DIRETORIAS ENCONTRADAS     : '.
           03  LT20-TOT-DIR-ENC        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT21.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DIRETORIAS NAO ENCONTRADAS : '.
           03  LT21-TOT-DIR-N-ENC      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT22.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DIRETORIAS ZERADAS NA SAIDA: '.
           03  LT22-TOT-DIR-ZEROS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT23.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DIFERENTES DE ZEROS SAIDA  : '.
           03  LT23-TOT-DIR-DIF-ZEROS  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT24.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT ACESSOS AO MESU9023        : '.
           03  LT24-TOT-ACESSO-MESU9023 PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT25.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT GERENCIAS ENCONTRADAS      : '.
           03  LT25-TOT-GER-ENC        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT26.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT GERENCIAS NAO ENCONTRADAS  : '.
           03  LT26-TOT-GER-N-ENC      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT27.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT GERENCIAS ZERADAS NA SAIDA : '.
           03  LT27-TOT-GER-ZEROS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT28.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT DIFERENTES DE ZEROS SAIDA  : '.
           03  LT28-TOT-GER-DIF-ZEROS  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINHIFEN.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(052)         VALUE
               '----------------------------------------------------'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO MESU9014 ***'.
      *----------------------------------------------------------------*

-INC I#MESUM4

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO GRUP0930 ***'.
      *----------------------------------------------------------------*

       COPY GRUPW01S.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

-INC POL7100C

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6216 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FS-MVTCHQSR     EQUAL '10'.

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  MVTCHQSR
                OUTPUT EMITCHEQ
                OUTPUT SRELCTRL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-MVTCHQSR.

           PERFORM 112000-TESTAR-FS-EMITCHEQ.

           PERFORM 113000-TESTAR-FS-SRELCTRL.


      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       111000-TESTAR-FS-MVTCHQSR       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MVTCHQSR         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'MVTCHQSR'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-MVTCHQSR   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-EMITCHEQ       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EMITCHEQ         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'EMITCHEQ'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-EMITCHEQ   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       113000-TESTAR-FS-SRELCTRL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SRELCTRL'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-SRELCTRL   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       114000-TESTAR-RETURN-CODE       SECTION.
      *----------------------------------------------------------------*

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  WRK-CADCLIE       TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-CADCLIE    TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-MVTCHQSR.

           IF  WRK-FS-MVTCHQSR         EQUAL '10'
               DISPLAY '***************** SLIG6216 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO MVTCHQSR VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6216 *****************'
               MOVE WRK-MSG001         TO ERR-TEXTO
VOLTAR         PERFORM 400000-FINALIZAR
VOLTAR*        P*RFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 220000-LER-CADCLIE.

           IF WRK-FS-CADCLIE   EQUAL  'S'  AND
              ACU-LIDOS-CADCLIE    NOT GREATER ZEROS
               DISPLAY '***************** SLIG6216 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO CADCLIE VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6216 *****************'
              MOVE  'APL'       TO  ERR-TIPO-ACESSO
              MOVE '                ARQUIVO CADCLIE VAZIO '
                                TO  ERR-TEXTO
              PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-LER-MVTCHQSR             SECTION.
      *----------------------------------------------------------------*

           READ MVTCHQSR.

           IF  WRK-FS-MVTCHQSR         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ATU
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-MVTCHQSR.

           MOVE SLIG28-BCO-CLI         TO WRK-BCO-ATU
           MOVE SLIG28-AGENCIA-CLI     TO WRK-AGE-ATU
           MOVE SLIG28-CONTA-CLI       TO WRK-CTA-ATU

           ADD 1                       TO ACU-LIDOS-MVTCHQSR.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       220000-LER-CADCLIE              SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0300'  USING  WRK-CADCLIE  REG-DEVE17.

           IF  RETURN-CODE     EQUAL 04
               MOVE  'S'               TO WRK-FS-CADCLIE
               MOVE 999                TO WRK-BANCO-DEVE
               MOVE 99999              TO WRK-AGENCIA-DEVE
               MOVE 9999999999999      TO WRK-CONTA-DEVE
               GO TO 220000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 114000-TESTAR-RETURN-CODE.

           ADD 1                       TO ACU-LIDOS-CADCLIE.

           MOVE DEVE17-BANCO       TO WRK-BANCO-DEVE.
           MOVE DEVE17-AGENCIA     TO WRK-AGENCIA-DEVE.
           MOVE DEVE17-CONTA       TO WRK-CONTA-DEVE.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-ATU NOT EQUAL  WRK-CHV-ANT

      *-----  FORMATA NOME E CNPJ/CPF DO CLIENTE A PARTIR DO CADACLIE -*

              MOVE ZEROS            TO  WRK-FLAG
                                        WRK-CGC-CPF-CLI
                                        WRK-FILIAL-CLI
                                        WRK-CONTROLE-CLI

              MOVE SPACES           TO  WRK-NOME-CLI

              PERFORM 3220000-OBTER-DADOS-CLIENTE UNTIL
                                         WRK-FLAG EQUAL 1 OR
                                         WRK-FLAG EQUAL 2 OR
                                   WRK-FS-CADCLIE EQUAL 'S'

              IF WRK-FS-CADCLIE EQUAL 'S' AND
                 WRK-NOME-CLI   EQUAL SPACES
                 MOVE 'CLIENTE NAO ENCONTRADO' TO WRK-NOME-CLI
              END-IF

              IF WRK-CGC-CPF-CLI   NOT EQUAL ZEROS

      *--------  FORMATA GRUPO ECONOMICO ATRAVES DO GRUP0930  ---------*

                 PERFORM 325000-OBTER-GRUPO-CLIENTE
              ELSE
                 MOVE ZEROS           TO WRK-GRP-ECONOMICO-CLI
              END-IF

              IF WRK-BCO-ATU EQUAL 237 AND
                (WRK-BCO-ATU NOT EQUAL WRK-BCO-ANT OR
                 WRK-AGE-ATU NOT EQUAL WRK-AGE-ANT)

      *--------  FORMATA DIRETORIA REGIONAL ATRAVES DA MESU9014  ------*

                 MOVE ZEROS       TO WRK-BCO-DIR-REGIONAL
                                     WRK-DIR-REGIONAL

                 PERFORM 323000-OBTER-DIRETORIA

      *--------  FORMATA GERENCIA REGIONAL ATRAVES DA MESU9023  -------*

                 MOVE ZEROS       TO WRK-BCO-GERC-REGIONAL
                                     WRK-GERC-REGIONAL

                 PERFORM 324000-OBTER-GERENCIA

              END-IF

              MOVE WRK-CHV-ATU TO  WRK-CHV-ANT
           END-IF.

           PERFORM 320000-GRAVAR-EMITCHEQ.
           PERFORM 210000-LER-MVTCHQSR.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       320000-GRAVAR-EMITCHEQ          SECTION.
      *----------------------------------------------------------------*

      *--  FORMATA CAMPOS DO CLIENTE PARA GRAVACAO  -------------------*

           MOVE WRK-GRP-ECONOMICO-CLI    TO SLIG28-GRP-ECONOMICO-CLI.
           MOVE WRK-CGC-CPF-CLI          TO SLIG28-CGC-CPF-CLI-ST.
           MOVE WRK-FILIAL-CLI           TO SLIG28-FILIAL-CLI-ST.
           MOVE WRK-CONTROLE-CLI         TO SLIG28-CONTROLE-CLI-ST.
           MOVE WRK-NOME-CLI             TO SLIG28-NOME-CLI.
           MOVE WRK-BCO-DIR-REGIONAL     TO SLIG28-BCO-DIR-REGIONAL.
           MOVE WRK-DIR-REGIONAL         TO SLIG28-DIR-REGIONAL.
           MOVE WRK-BCO-GERC-REGIONAL    TO SLIG28-BCO-GERC-REGIONAL.
           MOVE WRK-GERC-REGIONAL        TO SLIG28-GERC-REGIONAL.

      *--  FORMATA NOME EMITENTE CASO CNPJ/CPF SEJA IGUAL AO CLIENTE --*

           IF SLIG28-CGC-CPF-CLI-ST     EQUAL SLIG28-CGC-CPF-EMITT-ST
           AND
              SLIG28-FILIAL-CLI-ST      EQUAL SLIG28-FILIAL-EMITT-ST
              AND
              SLIG28-CONTROLE-CLI-ST    EQUAL SLIG28-CONTROLE-EMITT-ST
              AND
              SLIG28-NOME-CLI        NOT EQUAL SPACES             AND
              SLIG28-NOME-CLI        NOT EQUAL
                                     'CLIENTE NAO ENCONTRADO'     AND
              SLIG28-NOME-EMITT      NOT EQUAL SLIG28-NOME-CLI
              MOVE SLIG28-NOME-CLI       TO SLIG28-NOME-EMITT
           END-IF.

      *--  FORMATA TIPO DE PESSOA CLIENTE  ----------------------------*

           IF SLIG28-CGC-CPF-CLI-ST
           NOT EQUAL ZEROS AND
              SLIG28-FILIAL-CLI-ST
              EQUAL ZEROS
              MOVE 'F'                   TO SLIG28-TIPO-PESSOA-CLI
           ELSE
              MOVE 'J'                   TO SLIG28-TIPO-PESSOA-CLI
           END-IF.

      *--  ACUMULA GERENCIA REGIONAL CLIENTE ZERADA  ------------------*

           IF SLIG28-BCO-GERC-REGIONAL EQUAL ZEROS AND
              SLIG28-GERC-REGIONAL     EQUAL ZEROS
              ADD 1                       TO ACU-CLI-GER-ZEROS-OUT
           ELSE
              ADD 1                       TO ACU-CLI-GER-N-ZEROS-OUT
           END-IF.

      *--  ACUMULA DIRETORIA REGIONAL CLIENTE ZERADA  -----------------*

           IF SLIG28-BCO-DIR-REGIONAL  EQUAL ZEROS AND
              SLIG28-DIR-REGIONAL      EQUAL ZEROS
              ADD 1                       TO ACU-CLI-DIR-ZEROS-OUT
           ELSE
              ADD 1                       TO ACU-CLI-DIR-N-ZEROS-OUT
           END-IF.

      *--  ACUMULA GRUPO ECONOMICO CLIENTE ZERADO  --------------------*

           IF SLIG28-GRP-ECONOMICO-CLI EQUAL ZEROS
              ADD 1                      TO ACU-CLI-GRP-ZEROS-OUT
           ELSE
              ADD 1                      TO ACU-CLI-GRP-N-ZEROS-OUT
           END-IF.

      *--  ACUMULA CPF/CGC CLIENTE ZERADO  ----------------------------*

           IF SLIG28-CGC-CPF-CLI-ST
           EQUAL ZEROS
              ADD 1                      TO ACU-CLI-CPF-ZEROS-OUT
           ELSE
              ADD 1                      TO ACU-CLI-CPF-N-ZEROS-OUT
           END-IF.

      *--  ACUMULA NOME CLIENETE NAO ECONTRADO E SPACES ---------------*

           IF SLIG28-NOME-CLI         EQUAL 'CLIENTE NAO ENCONTRADO'
              ADD 1                      TO ACU-CLI-NOME-N-ENC-OUT
           ELSE
              IF SLIG28-NOME-CLI      EQUAL SPACES
                 ADD 1                   TO ACU-CLI-NOME-SPACES-OUT
              ELSE
                 ADD 1                   TO ACU-CLI-NOME-OK-OUT
              END-IF
           END-IF.

      *--  GRAVA REGISTRO EM EMITCHEQ  --------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-CHEQUE-SAIDA  FROM  REG-CHEQUE-EMITENTE.

           PERFORM 112000-TESTAR-FS-EMITCHEQ.

           ADD 1                       TO ACU-GRAVA-EMITCHEQ.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       3220000-OBTER-DADOS-CLIENTE      SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-ATU  EQUAL  WRK-CHV-DEVE17

      *-----  FORMATA NOME E CNPJ/CPF DO CLIENTE  ---------------------*

              MOVE DEVE17-PRINCIPAL-ST TO WRK-CGC-CPF-CLI
              MOVE DEVE17-FILIAL-ST(2:4)  TO WRK-FILIAL-CLI
              MOVE DEVE17-CTLE-ST     TO WRK-CONTROLE-CLI
              MOVE DEVE17-NOME-CLIE   TO WRK-NOME-CLI

              MOVE 1                  TO WRK-FLAG
              ADD  1                  TO ACU-CLI-NOME-CPF-ATLZ

           ELSE
              IF WRK-CHV-ATU GREATER  WRK-CHV-DEVE17

                 PERFORM 220000-LER-CADCLIE
                 GO TO   3220000-99-FIM

              ELSE
                 IF WRK-CHV-ATU  LESS  WRK-CHV-DEVE17

                    MOVE 'CLIENTE NAO ENCONTRADO' TO WRK-NOME-CLI
                    MOVE 2                        TO WRK-FLAG
                    ADD  1                        TO ACU-N-ENC-CLIB2000

                    IF ACU-N-ENC-CLIB2000 NOT GREATER 100
                       DISPLAY '%% - CHAVE NAO ENCONTRADA NO CADCLIE'
                               ' - BANCO: '   WRK-BCO-ATU
                               ' - AGENCIA: ' WRK-AGE-ATU
                               ' - CONTA: '   WRK-CTA-ATU
                    END-IF
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       323000-OBTER-DIRETORIA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     COMU-DADOS-9014.
           MOVE ZEROS                  TO RETURN-CODE.

           MOVE SLIG28-BCO-CLI         TO COMU-COD-EMPR.
           MOVE SLIG28-AGENCIA-CLI     TO COMU-COD-DEPDC.
           MOVE 'VRS001'               TO COMU-VERSAO.
           MOVE 'MESU9014'             TO WRK-MODULO.

           CALL WRK-MODULO             USING COMU-DADOS-9014.

           ADD 1                          TO ACU-ACESSOS-MESU9014.

           IF RETURN-CODE              EQUAL ZEROS

              MOVE SLIG28-BCO-CLI TO WRK-BCO-DIR-REGIONAL
              MOVE COMU-JUNC-DIR  TO WRK-DIR-REGIONAL
              ADD 1               TO ACU-MESU9014-DIR-ENC

           ELSE
              MOVE ZEROS               TO WRK-BCO-DIR-REGIONAL
                                          WRK-DIR-REGIONAL

              IF RETURN-CODE     EQUAL 8
                 ADD 1           TO ACU-MESU9014-DIR-N-ENC
              ELSE
                 ADD 1           TO ACU-ERROR-MESU9014

                 IF ACU-ERROR-MESU9014 LESS 101
                    DISPLAY '%% MESU9014 - BANCO: '       COMU-COD-EMPR
                                    ' - AGENCIA: '     COMU-COD-DEPDC
                                    ' - ERRO: ' COMU-MENSAGEM
                 END-IF
           END-IF.

      *----------------------------------------------------------------*
       323000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       324000-OBTER-GERENCIA           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-9023-PARM.

           MOVE 17                     TO WRK-9023-TPO-RELAC.
           MOVE 'SLIG6216'             TO WRK-9023-NOME-PGM.
           MOVE 'B'                    TO WRK-9023-AMBIENTE.

           MOVE SLIG28-BCO-CLI         TO WRK-9023-BANCO.
           MOVE SLIG28-AGENCIA-CLI     TO WRK-9023-AGENCIA.
           MOVE 'MESU9023'             TO WRK-MODULO.
           MOVE SPACES                 TO WRK-IO-PCB
                                          WRK-ALT-PCB
                                          WRK-9023-ERRO-AREA
                                          WRK-SQLCA.

           CALL WRK-MODULO          USING WRK-9023-AMBIENTE
                                          WRK-IO-PCB
                                          WRK-ALT-PCB
                                          WRK-9023-PARM
                                          WRK-9023-ERRO-AREA
                                          WRK-SQLCA.

           ADD 1                       TO ACU-ACESSOS-MESU9023.

           IF WRK-9023-COD-RET        EQUAL ZEROS

              MOVE WRK-9023-EMPR-RELAC   TO WRK-BCO-GERC-REGIONAL
              MOVE WRK-9023-DEPTO-RELAC  TO WRK-GERC-REGIONAL

              ADD 1                      TO ACU-MESU9023-GER-ENC
           ELSE

              MOVE ZEROS                 TO WRK-BCO-GERC-REGIONAL
                                            WRK-GERC-REGIONAL

              IF WRK-9023-COD-RET    NOT EQUAL 1
                 ADD 1                   TO ACU-ERROR-MESU9023

                 IF ACU-ERROR-MESU9023    LESS 101

                    IF WRK-9023-COD-RET EQUAL 2
                       MOVE 'DADOS ENVIADOS INCONSISTENTES'
                                       TO WRK-MESU9023-MSG-ERRO
                    ELSE
                       MOVE 'ERRO DB2' TO WRK-MESU9023-MSG-ERRO
                    END-IF

                    DISPLAY
                    '%% MESU9023 - BANCO: ' WRK-9023-BANCO
                    ' - AGENCIA: ' WRK-9023-AGENCIA
                    ' - ERRO: ' WRK-9023-COD-RET
                    '-' WRK-MESU9023-MSG-ERRO

                 END-IF
              ELSE
                 ADD 1                      TO ACU-MESU9023-GER-N-ENC
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       324000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       325000-OBTER-GRUPO-CLIENTE      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                        GRUPW01S-REG-EMPRESA.

           MOVE '00'                      TO GRUPW01S-CGC-CPF(1:2)
           MOVE WRK-CGC-CPF-CLI           TO GRUPW01S-CGC-CPF(3:9)
           MOVE WRK-FILIAL-CLI            TO GRUPW01S-FILIAL-CGC-CPF.
           MOVE WRK-CONTROLE-CLI          TO GRUPW01S-CTRL-CGC-CPF.
           MOVE ZEROS                     TO GRUPW01S-NUMERO-GRUPO-ENV.

           ADD  1                         TO ACU-ACESSOS-GRUP0930.

           PERFORM 325100-ACESSAR-GRUP0930.

           IF  GRUPW01S-RETURN-CODE     EQUAL ZEROS
               ADD  1                     TO ACU-GRUP0930-GRP-ENC
               MOVE GRUPW01S-NUMERO-GRUPO-RET
                                          TO WRK-GRP-ECONOMICO-CLI
           ELSE
               MOVE ZEROS                 TO WRK-GRP-ECONOMICO-CLI

               IF  GRUPW01S-RETURN-CODE   NOT EQUAL 1
                   ADD  1                 TO ACU-GRUP0930-ERRO

                   IF ACU-GRUP0930-ERRO NOT GREATER 100

                      IF GRUPW01S-RETURN-CODE  EQUAL 4
                         MOVE 'ARGUMENTO DE PESQUISA INCONSISTENTE'
                                         TO WRK-GRUP0930-MSG-ERRO
                      ELSE
                         MOVE 'ERRO DB2' TO WRK-GRUP0930-MSG-ERRO
                      END-IF

                      DISPLAY '%% GRUP0930 '
                              ' - CLIENTE CNPJ/CPF: ' WRK-CGC-CPF-CLI
                              '/' WRK-FILIAL-CLI '-' WRK-CONTROLE-CLI
                              ' - ERRO : ' GRUPW01S-RETURN-CODE
                              '-' WRK-GRUP0930-MSG-ERRO

                   END-IF
               ELSE
                   ADD  1                 TO ACU-GRUP0930-GRP-N-ENC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       325000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       325100-ACESSAR-GRUP0930         SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG6216'             TO GRUPW01S-PGM-CHAMADOR.
           MOVE 'GRUPW01S'             TO WRK-MODULO.
           MOVE SPACES                 TO WRK-IO-PCB
                                          WRK-ALT-PCB.

           CALL WRK-MODULO             USING GRUPW01S-REG-EMPRESA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.


      *----------------------------------------------------------------*
       325100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 410000-EMITIR-SRELCTRL.

           CLOSE MVTCHQSR
                 EMITCHEQ
                 SRELCTRL.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       410000-EMITIR-SRELCTRL          SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'              USING WRK-DATA-HORA.

           MOVE WRK-P7600-DIA           TO CB2-DIA.
           MOVE WRK-P7600-MES           TO CB2-MES.
           MOVE WRK-P7600-ANO           TO CB2-ANO.
           MOVE ACU-LIDOS-MVTCHQSR      TO LT1-TOT-LIDOS.
           MOVE ACU-GRAVA-EMITCHEQ      TO LT2-TOT-GRAVADOS.
           MOVE ACU-ERROR-MESU9014      TO LT3-TOT-GRAVADOS.
           MOVE ACU-ERROR-MESU9023      TO LT4-TOT-GRAVADOS.
           MOVE ACU-LIDOS-CADCLIE       TO LT5-TOT-LIDOS.
           MOVE ACU-N-ENC-CLIB2000      TO LT6-TOT-GRAVADOS.
           MOVE ACU-N-ENC-CLIB2000      TO LT6-TOT-GRAVADOS.
           MOVE ACU-CLI-NOME-CPF-ATLZ   TO LT7-TOT-CLIE-ATUALZ.
           MOVE ACU-CLI-CPF-ZEROS-OUT   TO LT8-TOT-CNPJ-ZEROS.
           MOVE ACU-CLI-CPF-N-ZEROS-OUT TO LT9-TOT-CNPJ-DIF-ZEROS.
           MOVE ACU-CLI-NOME-N-ENC-OUT  TO LT10-TOT-NOME-N-ENC.
           MOVE ACU-CLI-NOME-SPACES-OUT TO LT11-TOT-NOME-SPACES.
           MOVE ACU-CLI-NOME-OK-OUT     TO LT12-TOT-NOME-OK.
           MOVE ACU-ACESSOS-GRUP0930    TO LT13-TOT-ACESSO-GRUP0930.
           MOVE ACU-GRUP0930-ERRO       TO LT14-TOT-ERROS-GRUP0930.
           MOVE ACU-GRUP0930-GRP-ENC    TO LT15-TOT-ENC-GRUP0930.
           MOVE ACU-GRUP0930-GRP-N-ENC  TO LT16-TOT-N-ENC-GRUP0930.
           MOVE ACU-CLI-GRP-ZEROS-OUT   TO LT17-TOT-GRP-ZEROS.
           MOVE ACU-CLI-GRP-N-ZEROS-OUT TO LT18-TOT-GRP-DIF-ZEROS.
           MOVE ACU-ACESSOS-MESU9014    TO LT19-TOT-ACESSO-MESU9014.
           MOVE ACU-MESU9014-DIR-ENC    TO LT20-TOT-DIR-ENC.
           MOVE ACU-MESU9014-DIR-N-ENC  TO LT21-TOT-DIR-N-ENC.
           MOVE ACU-CLI-DIR-ZEROS-OUT   TO LT22-TOT-DIR-ZEROS.
           MOVE ACU-CLI-DIR-N-ZEROS-OUT TO LT23-TOT-DIR-DIF-ZEROS.
           MOVE ACU-ACESSOS-MESU9023    TO LT24-TOT-ACESSO-MESU9023.
           MOVE ACU-MESU9023-GER-ENC    TO LT25-TOT-GER-ENC.
           MOVE ACU-MESU9023-GER-N-ENC  TO LT26-TOT-GER-N-ENC.
           MOVE ACU-CLI-GER-ZEROS-OUT   TO LT27-TOT-GER-ZEROS.
           MOVE ACU-CLI-GER-N-ZEROS-OUT TO LT28-TOT-GER-DIF-ZEROS.

      *--  GRAVA CABECALHO  -------------------------------------------*

           WRITE REG-SRELCTRL          FROM CABEC1.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC2.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC3.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC4.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

      *--  LIDOS E GRAVADOS EM MVTCHQSR / EMITCHEQ  -------------------*

           WRITE REG-SRELCTRL          FROM LINTOT1.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

      *--  TOTAIS DO CADACLIE  ----------------------------------------*

           WRITE REG-SRELCTRL          FROM LINTOT5.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT6.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT7.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT8.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT9.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT10.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT11.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT12.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

      *--  TOTAIS DO GRUP0930  ----------------------------------------*

           WRITE REG-SRELCTRL          FROM LINTOT13.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT14.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT15.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT16.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT17.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT18.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

      *--  TOTAIS DA MESU9014  ----------------------------------------*

           WRITE REG-SRELCTRL          FROM LINTOT19.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT3.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT20.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT21.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT22.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT23.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

      *--  TOTAIS DA MESU9023  ----------------------------------------*

           WRITE REG-SRELCTRL          FROM LINTOT24.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT4.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT25.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT26.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT27.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT28.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG6216'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             WRK-SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
