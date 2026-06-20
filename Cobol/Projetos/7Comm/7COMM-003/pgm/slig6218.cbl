      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG6218.
       AUTHOR.     REGINALDO PEDROSO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG6218                                    *
      *    ANALISTA....:   REGINALDO PEDROSO         - DTS / GP. 75    *
      *    DATA........:   MARCO/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   OBTEM NOME DO EMITENTE.                     *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                    MVTCHQSR                    I#SLIG28        *
      *                    CADCLIE                     I#DEVEN7        *
      *                    EMITCHEQ                    I#SLIG28        *
      *                    SRELCTRL                       -            *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0300 - LEITURA ARQUIVO COMPRIMIDO                       *
      *    POOL7100 - TRATAMENTO DE ERROS                              *
FS0417*================================================================*
'     *                    A  L  T  E  R  A  C  A  O                   *
'     *----------------------------------------------------------------*
'     *    ANALISTA....:   HENRIQUE MANDROTE         - FOURSYS         *
'     *    DATA........:   ABRIL/2017                                  *
'     *    OBJETIVO....:   ACERTO PARA MOVER FILIAL NA CHAVE DE        *
'     *                    COMPARACAO. HOUVE CASOS DE CLIENTES COM     *
'     *                    BASE DE CPF/CNPJ IGUAIS, MAS FILIAIS        *
'     *                    DIFERENTES                                  *
FS0417*================================================================*
FS1217*================================================================*
'     *                    A  L  T  E  R  A  C  A  O                   *
'     *----------------------------------------------------------------*
'     *    ANALISTA....:   HENRIQUE MANDROTE         - FOURSYS         *
'     *    DATA........:   DEZ/2017                                    *
'     *    OBJETIVO....:   RETIRAR ABEND QUANDO ARQUIVO VAZIO          *
FS1217*================================================================*
FS0618*================================================================*
'     *                    A  L  T  E  R  A  C  A  O                   *
'     *----------------------------------------------------------------*
'     *    ANALISTA....:   HENRIQUE MANDROTE         - FOURSYS         *
'     *    DATA........:   JUNHO/2018                                  *
'     *    OBJETIVO....:   RETIRAR REGRA QUE ZERA EMITENTE CASO        *
'     *                    NAO ESTEJA CADASTRADO NO CLIE               *
FS0618*================================================================*
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
           '*** SLIG6218 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-MVTCHQSR          PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-EMITCHEQ          PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CADCLIE           PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-N-ENC-CLIB2000          PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMT-NOME-CPF-ATLZ       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMT-NOME-SPACES-OUT     PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMT-NOME-OK-OUT         PIC  9(015) COMP-3  VALUE ZEROS.
       77  WRK-FLAG                    PIC  9(001)         VALUE ZEROS.

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

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-CADCLIE                 PIC  X(008)         VALUE
           'CADCLIE'.

       77  WRK-NOME-EMI                PIC  X(040)         VALUE SPACES.
DTSFC  77  WRK-DEVE17-PRINCIPAL-ANT    PIC  X(009)         VALUE ZEROS.
DTSFC  77  WRK-CPFCGC-EMI              PIC  X(009)         VALUE ZEROS.
CNPJ7C 77  WRK-CPFCGC-EMI-N  REDEFINES
CNPJ7C     WRK-CPFCGC-EMI              PIC  9(009).
DTSFC  77  WRK-FILIAL-EMI              PIC  X(004)         VALUE ZEROS.
CNPJ7C 77  WRK-FILIAL-EMI-N  REDEFINES
CNPJ7C     WRK-FILIAL-EMI              PIC  9(004).
DTSFC  77  WRK-CONTRL-EMI              PIC  9(002) COMP-3  VALUE ZEROS.
DTSFC  77  WRK-TIPOPES-EMI             PIC  X(001)         VALUE SPACES.

       01  WRK-CHV-DEVE17.
           03  WRK-CPFCGC-DEVE         PIC  X(009)         VALUE ZEROS.
           03  WRK-FILIAL-DEVE         PIC  X(005)         VALUE ZEROS.

       01  WRK-CHV-ATU.
           03  WRK-CPFCGC-ATU          PIC  X(009)         VALUE ZEROS.
           03  WRK-FILIAL-ATU          PIC  X(004)         VALUE ZEROS.
FS0618 01  WRK-CONTRL-ATU              PIC  9(002)         VALUE ZEROS.

       01  WRK-CHV-ANT.
           03  WRK-CPFCGC-ANT          PIC  X(009)    VALUE '999999999'.
           03  WRK-FILIAL-ANT          PIC  X(004)         VALUE ZEROS.

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
               'SLIG6218'.
           03  CB2-DIA                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MES                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-ANO                 PIC  9(004)         VALUE ZEROS.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(043)         VALUE
               'OBTEM DADOS DO EMITENTE'.

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
               'TOT EMITENTE NOME/CNPJ ATUALZ  : '.
           03  LT7-TOT-CLIE-ATUALZ     PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.


       01  LINTOT11.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT EMITENTE NOME SPACES SAIDA : '.
           03  LT11-TOT-NOME-SPACES    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT12.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE
               'TOT EMITENTE NOME OK SAIDA     : '.
           03  LT12-TOT-NOME-OK        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINHIFEN.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(052)         VALUE
               '----------------------------------------------------'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

-INC POL7100C

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6218 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           MOVE   '999999999'         TO WRK-DEVE17-PRINCIPAL-ANT.

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
               DISPLAY '***************** SLIG6218 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO MVTCHQSR VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6218 *****************'
               MOVE WRK-MSG001         TO ERR-TEXTO
***************X_RFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 220000-LER-CADCLIE.

           IF WRK-FS-CADCLIE   EQUAL  'S'  AND
              ACU-LIDOS-CADCLIE    NOT GREATER ZEROS
               DISPLAY '***************** SLIG6218 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO CADCLIE VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6218 *****************'
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

           ADD 1                       TO ACU-LIDOS-MVTCHQSR.

DTSFC      IF  SLIG28-BCO-EMITT  EQUAL  237
DTSFC          MOVE  WRK-GRAVACAO  TO  WRK-OPERACAO
DTSFC          WRITE  REG-CHEQUE-SAIDA  FROM  REG-CHEQUE-EMITENTE
DTSFC          PERFORM  112000-TESTAR-FS-EMITCHEQ
DTSFC          ADD  1              TO  ACU-GRAVA-EMITCHEQ
DTSFC          GO TO  210000-LER-MVTCHQSR
DTSFC      END-IF.

           MOVE SLIG28-CGC-CPF-EMITT-ST
           TO WRK-CPFCGC-ATU.
FS0417     MOVE SLIG28-FILIAL-EMITT-ST   TO WRK-FILIAL-ATU.
FS0618     MOVE SLIG28-CONTROLE-EMITT-ST  TO WRK-CONTRL-ATU.

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
               MOVE HIGH-VALUES        TO WRK-CHV-DEVE17
               GO TO 220000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 114000-TESTAR-RETURN-CODE.

           ADD 1                       TO ACU-LIDOS-CADCLIE.

      *--  FORMATA TIPO DE PESSOA PARA ARQUIVO CADACLIE  --------------*

           IF DEVE17-PRINCIPAL-ST EQUAL ZEROS
              MOVE 'J'                 TO DEVE17-TP-PESSOA
           ELSE
CNPJ7C         IF DEVE17-FILIAL-ST EQUAL ZEROS OR SPACES
                  MOVE 'F'             TO DEVE17-TP-PESSOA
               ELSE
                  MOVE 'J'             TO DEVE17-TP-PESSOA
               END-IF
           END-IF.

      *--  VERIFICA TIPOS DIFERENTES PARA O MESMO CORPO DE CNPJ/CPF ---*

DTSFC      IF  DEVE17-PRINCIPAL-ST  EQUAL  WRK-DEVE17-PRINCIPAL-ANT
DTSFC          IF  DEVE17-TP-PESSOA  EQUAL  WRK-TIPOPES-EMI
DTSFC              GO  TO  220000-LER-CADCLIE
FS0618*        XLSE

      *----------- QUANDO TIPO DIFERENTES PARA O MESMO CORPO ZERA -----*
      *----------- EMITENTE  ------------------------------------------*

FS0618*            XOVE SPACES         TO  WRK-NOME-EMI
FS0618*            XOVE ZEROS          TO  WRK-CPFCGC-EMI
FS0618*                                    WRK-FILIAL-EMI
FS0618*                                    WRK-CONTRL-EMI
FS0618*            XOVE 'J'            TO  WRK-TIPOPES-EMI
DTSFC          END-IF
DTSFC      END-IF.

           MOVE DEVE17-PRINCIPAL-ST    TO WRK-CPFCGC-DEVE.
FS0417     MOVE DEVE17-FILIAL-ST       TO WRK-FILIAL-DEVE.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-ATU NOT EQUAL  WRK-CHV-ANT

      *-----  FORMATA NOME DO EMITENTE A PARTIR DO CADACLIE  ---------*

              MOVE ZEROS            TO  WRK-FLAG
              MOVE SPACES           TO  WRK-NOME-EMI

              PERFORM 3220000-OBTER-DADOS-EMITENTE UNTIL
                                          WRK-FLAG EQUAL 1 OR
                                          WRK-FLAG EQUAL 2 OR
                                    WRK-FS-CADCLIE EQUAL 'S'

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

           MOVE WRK-NOME-EMI             TO SLIG28-NOME-EMITT.

CNPJ7C     IF (WRK-CPFCGC-EMI  NUMERIC) AND
CNPJ7C        (WRK-FILIAL-EMI  NUMERIC)
CNPJ7C         MOVE WRK-CPFCGC-EMI-N     TO SLIG28-CGC-CPF-EMITT
CNPJ7C         MOVE WRK-FILIAL-EMI-N     TO SLIG28-FILIAL-EMITT
CNPJ7C         MOVE WRK-CPFCGC-EMI       TO SLIG28-CGC-CPF-EMITT
CNPJ7C     ELSE
CNPJ7C         MOVE ZEROS                TO SLIG28-CGC-CPF-EMITT
CNPJ7C                                      SLIG28-FILIAL-EMITT
CNPJ7C                                      SLIG28-CGC-CPF-EMITT
CNPJ7C     END-IF

DTSFC      MOVE WRK-CPFCGC-EMI           TO SLIG28-CGC-CPF-EMITT-ST.
DTSFC      MOVE WRK-FILIAL-EMI           TO SLIG28-FILIAL-EMITT-ST.
DTSFC      MOVE WRK-CONTRL-EMI           TO SLIG28-CONTROLE-EMITT-ST.
DTSFC      MOVE WRK-TIPOPES-EMI          TO SLIG28-TIPO-PESSOA-EMITT.

           IF SLIG28-NOME-EMITT    EQUAL SPACES
              ADD 1                   TO ACU-EMT-NOME-SPACES-OUT
           ELSE
              ADD 1                   TO ACU-EMT-NOME-OK-OUT
           END-IF

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
       3220000-OBTER-DADOS-EMITENTE      SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-ATU  EQUAL  WRK-CHV-DEVE17

              MOVE DEVE17-NOME-CLIE  TO WRK-NOME-EMI
DTSFC         MOVE DEVE17-PRINCIPAL-ST  TO WRK-CPFCGC-EMI
DTSFC                                   WRK-DEVE17-PRINCIPAL-ANT
DTSFC         MOVE DEVE17-FILIAL-ST(2:4) TO WRK-FILIAL-EMI
DTSFC         MOVE DEVE17-CTLE-ST    TO WRK-CONTRL-EMI
DTSFC         MOVE DEVE17-TP-PESSOA  TO WRK-TIPOPES-EMI

DTSFC         PERFORM 220000-LER-CADCLIE

      *-----  FORMATA NOME DO EMITENTE  -------------------------------*

              MOVE 1                  TO WRK-FLAG
              ADD  1                  TO ACU-EMT-NOME-CPF-ATLZ

           ELSE
              IF WRK-CHV-ATU GREATER  WRK-CHV-DEVE17

                 PERFORM 220000-LER-CADCLIE
                 GO TO   3220000-99-FIM

              ELSE
                 IF WRK-CHV-ATU  LESS  WRK-CHV-DEVE17

                    MOVE SPACES                   TO WRK-NOME-EMI
                    MOVE 2                        TO WRK-FLAG
                    ADD  1                        TO ACU-N-ENC-CLIB2000

FS0618              MOVE 'NAO CADASTRADO'
'                                       TO WRK-NOME-EMI
'
'                   MOVE WRK-CPFCGC-ATU TO WRK-CPFCGC-EMI
'                   MOVE WRK-FILIAL-ATU TO WRK-FILIAL-EMI
'                   MOVE WRK-CONTRL-ATU TO WRK-CONTRL-EMI
'
CNPJ7C              IF WRK-FILIAL-ATU   EQUAL ZEROS OR SPACES
'                      MOVE 'F'         TO WRK-TIPOPES-EMI
'                   ELSE
'                      MOVE 'J'         TO WRK-TIPOPES-EMI
FS0618              END-IF

                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3220000-99-FIM.                  EXIT.
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
           MOVE ACU-LIDOS-CADCLIE       TO LT5-TOT-LIDOS.
           MOVE ACU-N-ENC-CLIB2000      TO LT6-TOT-GRAVADOS.
           MOVE ACU-EMT-NOME-CPF-ATLZ   TO LT7-TOT-CLIE-ATUALZ.
           MOVE ACU-EMT-NOME-SPACES-OUT TO LT11-TOT-NOME-SPACES.
           MOVE ACU-EMT-NOME-OK-OUT     TO LT12-TOT-NOME-OK.

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

           WRITE REG-SRELCTRL          FROM LINTOT11.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT12.
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

           MOVE 'SLIG6218'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
