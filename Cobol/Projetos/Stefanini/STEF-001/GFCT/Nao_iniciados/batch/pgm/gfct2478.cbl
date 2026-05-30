      *================================================================*
       IDENTIFICATION DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2478.
       AUTHOR. VINICIUS.
      *================================================================*
      *                        WIPRO INFOOSERVER                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT2478                                     *
      *    PROGRAMADOR  : VINICIUS                                     *
      *    ANALISTA     : VINICIUS                                     *
      *    DATA         : FEV-2018                                     *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR ARQUIVO ACUMULADO DAS CESTAS NEXT JA ENCERRADAS     *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                INCLUDE/BOOK        *
      *      PASTNCAD                                --------          *
      *      PASTNMOV                                --------          *
      *      PASTNSAI                                --------          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL7100 - MODULO DE TRATAMENTO DE ERROS                  *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT PASTNCAD    ASSIGN TO UT-S-PASTNCAD
           FILE STATUS               IS WRK-FS-PASTNCAD.

           SELECT PASTNMOV    ASSIGN TO UT-S-PASTNMOV
           FILE STATUS               IS WRK-FS-PASTNMOV.

           SELECT PASTNSAI    ASSIGN TO UT-S-PASTNSAI
           FILE STATUS               IS WRK-FS-PASTNSAI.

      *================================================================*
       DATA DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  INPUT....: LAYOUT DO ARQUIVO                                  *
      *             ORG. SEQUENCIAL                                    *
      *----------------------------------------------------------------*

       FD  PASTNCAD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-PASTNCAD-REG.
           05  FD-ADESAO-REG-C.
               15 FD-CJUNC-DEPDC-C     PIC S9(5)V  COMP-3.
               15 FD-CCTA-CLI-C        PIC S9(7)V  COMP-3.
               15 FD-CSERVC-TARIF-C    PIC S9(5)V  COMP-3.
               15 FD-HINCL-REG-C       PIC X(26).
               15 FILLER               PIC X(58).
ST25X6*           05  FD-CPF-CNPJ-C           PIC 9(009).
ST25X6*           05  FD-FILIAL-C             PIC 9(005).
ST25X6*           05  FD-CONTROLE-C           PIC 9(002).
ST25X6     05  FD-CPF-CNPJ-C           PIC X(009).
ST25X6     05  FD-FILIAL-C             PIC X(004).
ST25X6     05  FD-CONTROLE-C           PIC 9(002).
ST25X6* para manter o tamanho total do registro
ST25X6     05  FILLER                  PIC X(001).

      *----------------------------------------------------------------*
      *  INPUT....: LAYOUT DO ARQUIVO                                  *
      *             ORG. SEQUENCIAL                                    *
      *----------------------------------------------------------------*

       FD  PASTNMOV
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-PASTNMOV-REG.
           05  FD-ADESAO-REG-M.
               15 FD-CJUNC-DEPDC-M     PIC S9(5)V  COMP-3.
               15 FD-CCTA-CLI-M        PIC S9(7)V  COMP-3.
               15 FD-CSERVC-TARIF-M    PIC S9(5)V  COMP-3.
               15 FD-HINCL-REG-M       PIC X(26).
               15 FILLER               PIC X(58).
ST25X6*           05  FD-CPF-CNPJ-M           PIC 9(009).
ST25X6*           05  FD-FILIAL-M             PIC 9(005).
ST25X6*           05  FD-CONTROLE-M           PIC 9(002).
ST25X6     05  FD-CPF-CNPJ-M           PIC X(009).
ST25X6     05  FD-FILIAL-M             PIC X(004).
ST25X6     05  FD-CONTROLE-M           PIC 9(002).
ST25X6*  para manter o tamanho total do registro
ST25X6     05  FILLER                  PIC X(001).

      *----------------------------------------------------------------*
      *  OUTPUT...: LAYOUT DO ARQUIVO                                  *
      *             ORG. SEQUENCIAL                                    *
      *----------------------------------------------------------------*

       FD  PASTNSAI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-PASTNSAI-REG              PIC  X(110).

      *================================================================*
       WORKING-STORAGE SECTION.
      *================================================================*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           'INICIO DA WORKING STORAGE GFCT2478'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032) VALUE
           'AREAS AUXILIARES'.
      *----------------------------------------------------------------*

       01  WRK-FRWK2999                PIC  X(008)         VALUE
           'FRWK2999'.
       01  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-PASTNCAD-FIM            PIC  X(001)         VALUE SPACES.
       01  WRK-PASTNMOV-FIM            PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032) VALUE
           'AREAS PARA CHAVES'.
      *----------------------------------------------------------------*

       01 WRK-CHAVE-PASTNCAD.
         10 WRK-CAD-CJUNC-DEPDC        PIC S9(005) COMP-3 VALUE ZEROS.
         10 WRK-CAD-CCTA-CLI           PIC S9(007) COMP-3 VALUE ZEROS.
         10 WRK-CAD-CSERVC-TARIF       PIC S9(005) COMP-3 VALUE ZEROS.
         10 WRK-CAD-HINCL-REG          PIC  X(026)        VALUE SPACES.

       01 WRK-CHAVE-PASTNMOV.
         10 WRK-MOV-CJUNC-DEPDC        PIC S9(005) COMP-3 VALUE ZEROS.
         10 WRK-MOV-CCTA-CLI           PIC S9(007) COMP-3 VALUE ZEROS.
         10 WRK-MOV-CSERVC-TARIF       PIC S9(005) COMP-3 VALUE ZEROS.
         10 WRK-MOV-HINCL-REG          PIC  X(026)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'AREAS ACUMULADORES'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS-PASTNCAD          PIC  9(012)  COMP-3  VALUE ZEROS.
       01  ACU-LIDOS-PASTNMOV          PIC  9(012)  COMP-3  VALUE ZEROS.
       01  ACU-GRAVA-PASTNSAI          PIC  9(012)  COMP-3  VALUE ZEROS.

       01  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032) VALUE
           '* AREA PARA FRWK2999 *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-FRWK2999.
           COPY 'I#FRWKGE'.
           05  WRK-BLOCO-INFO-ERRO.
               10  WRK-CHAR-INFO-ERRO  PIC  X(001)
                                       OCCURS 0 TO 30000 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032) VALUE
           '* AREA PARA ERRO DE ARQUIVO *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-FRWKGARQ.
           COPY 'I#FRWKAR'.

      *----------------------------------------------------------------*
       01  FILLER                     PIC  X(050) VALUE
           'AREAS PARA TESTE DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-FS-PASTNCAD            PIC  X(002) VALUE SPACES.
       01  WRK-FS-PASTNMOV            PIC  X(002) VALUE SPACES.
       01  WRK-FS-PASTNSAI            PIC  X(002) VALUE SPACES.

       01  WRK-ABERTURA               PIC  X(013) VALUE 'NA ABERTURA'.
       01  WRK-LEITURA                PIC  X(013) VALUE 'NA LEITURA'.
       01  WRK-GRAVACAO               PIC  X(013) VALUE 'NA GRAVACAO'.
       01  WRK-FECHAMENTO             PIC  X(013) VALUE 'NO FECHAMENTO'.

       01  WRK-MENSAGEM-ERRO.
           05  FILLER                 PIC  X(009) VALUE '*** ERRO '.
           05  WRK-OPERACAO           PIC  X(013) VALUE SPACES.
           05  FILLER                 PIC  X(012) VALUE ' DO ARQUIVO '.
           05  WRK-NOME-ARQUIVO       PIC  X(008) VALUE SPACES.
           05  FILLER                 PIC  X(017) VALUE
               ' - FILE STATUS = '.
           05  WRK-FS                 PIC  X(002) VALUE SPACES.
           05  FILLER                 PIC  X(004) VALUE ' ***'.
           05  FILLER                 PIC  X(010) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'AREA DA COPYBOOK DA BRAD7100'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'FIM DA WORKING GFCT2478'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA DE INICIO DO PROCESSAMENTO.                             *
      *----------------------------------------------------------------*
       0000-INICIAR SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR   UNTIL
                   WRK-PASTNMOV-FIM EQUAL 'S' AND
                   WRK-PASTNCAD-FIM EQUAL 'S'.

           PERFORM 7000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA ABRIR ARQUIVOS E TESTAR FILE-STATUS.               *
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGARQ-REGISTRO.

           OPEN INPUT  PASTNCAD
                       PASTNMOV
                OUTPUT PASTNSAI.

           MOVE WRK-ABERTURA TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TESTAR FILE-STATUS DOS ARQUIVOS.                   *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-PASTNCAD.

           PERFORM 1120-TESTAR-FS-PASTNMOV.

           PERFORM 1130-TESTAR-FS-PASTNSAI.

      *----------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TESTAR FILE-STATUS DO ARQUIVO.                     *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-PASTNCAD SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-PASTNCAD NOT EQUAL '00'
               DISPLAY '************** GFCT2478 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              PASTNCAD             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-PASTNCAD
                                                  '         *'
               DISPLAY '************** GFCT2478 *************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE 'PASTNCAD'          TO WRK-NOME-ARQUIVO
                                           FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-PASTNCAD     TO WRK-FS
                                           FRWKGARQ-FILE-STATUS
               MOVE WRK-MENSAGEM-ERRO   TO ERR-TEXTO
               PERFORM  9998-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TESTAR FILE-STATUS DO ARQUIVO.                     *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-PASTNMOV SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-PASTNMOV NOT EQUAL '00'
               DISPLAY '************** GFCT2478 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              PASTNMOV             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-PASTNMOV
                                                  '         *'
               DISPLAY '************** GFCT2478 *************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE 'PASTNMOV'          TO WRK-NOME-ARQUIVO
                                           FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-PASTNMOV     TO WRK-FS
                                           FRWKGARQ-FILE-STATUS
               MOVE WRK-MENSAGEM-ERRO   TO ERR-TEXTO
               PERFORM  9998-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TESTAR FILE-STATUS DO ARQUIVO.                     *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-PASTNSAI SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-PASTNSAI NOT EQUAL '00'
               DISPLAY '************** GFCT2478 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              PASTNSAI             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-PASTNSAI
                                                  '         *'
               DISPLAY '************** GFCT2478 *************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE 'PASTNSAI'          TO WRK-NOME-ARQUIVO
                                           FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-PASTNSAI     TO WRK-FS
                                           FRWKGARQ-FILE-STATUS
               MOVE WRK-MENSAGEM-ERRO   TO ERR-TEXTO
               PERFORM  9998-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA VERIFICAR ARQUIVO VAZIO.                           *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-PASTNCAD.

           IF  ACU-LIDOS-PASTNCAD EQUAL ZEROS
               DISPLAY  '********* GFCT2478 *********'
               DISPLAY  '*     ARQUIVO  PASTNCAD    *'
               DISPLAY  '*          VAZIO.          *'
               DISPLAY  '********* GFCT2478 *********'
           END-IF.

           PERFORM 2200-LER-PASTNMOV.

           IF  ACU-LIDOS-PASTNMOV EQUAL ZEROS
               DISPLAY  '********* GFCT2478 *********'
               DISPLAY  '*     ARQUIVO  PASTNMOV    *'
               DISPLAY  '*          VAZIO.          *'
               DISPLAY  '********* GFCT2478 *********'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA LER ARQUIVO.                                       *
      *----------------------------------------------------------------*
       2100-LER-PASTNCAD SECTION.
      *----------------------------------------------------------------*

           READ PASTNCAD.

           IF  WRK-FS-PASTNCAD EQUAL '10'
               MOVE 'S'
                 TO WRK-PASTNCAD-FIM
               MOVE HIGH-VALUES
                 TO WRK-CHAVE-PASTNCAD
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-PASTNCAD.

           MOVE FD-CJUNC-DEPDC-C
           TO   WRK-CAD-CJUNC-DEPDC.

           MOVE FD-CCTA-CLI-C
           TO   WRK-CAD-CCTA-CLI.

           MOVE FD-CSERVC-TARIF-C
           TO   WRK-CAD-CSERVC-TARIF.

           MOVE FD-HINCL-REG-C
           TO   WRK-CAD-HINCL-REG.

           ADD 1
           TO  ACU-LIDOS-PASTNCAD.

      *----------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA LER ARQUIVO.                                       *
      *----------------------------------------------------------------*
       2200-LER-PASTNMOV SECTION.
      *----------------------------------------------------------------*

           READ PASTNMOV.

           IF  WRK-FS-PASTNMOV EQUAL '10'
               MOVE 'S'
                 TO WRK-PASTNMOV-FIM
               MOVE HIGH-VALUES
                 TO WRK-CHAVE-PASTNMOV
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-PASTNMOV.

           MOVE FD-CJUNC-DEPDC-M
           TO   WRK-MOV-CJUNC-DEPDC.

           MOVE FD-CCTA-CLI-M
           TO   WRK-MOV-CCTA-CLI.

           MOVE FD-CSERVC-TARIF-M
           TO   WRK-MOV-CSERVC-TARIF.

           MOVE FD-HINCL-REG-M
           TO   WRK-MOV-HINCL-REG.

           ADD 1
           TO  ACU-LIDOS-PASTNMOV.

      *----------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO.                                       *
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-PASTNCAD     EQUAL WRK-CHAVE-PASTNMOV
                 PERFORM 5100-GRAVAR-MOV-PASTNSAI
                 PERFORM 2200-LER-PASTNMOV
                 PERFORM 2100-LER-PASTNCAD
           ELSE
               IF  WRK-CHAVE-PASTNCAD GREATER WRK-CHAVE-PASTNMOV
                   PERFORM 5100-GRAVAR-MOV-PASTNSAI
                   PERFORM 2200-LER-PASTNMOV
               ELSE
                   PERFORM 5200-GRAVAR-CAD-PASTNSAI
                   PERFORM 2100-LER-PASTNCAD
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINAS PARA GRAVACAO DE ARQUIVO.                              *
      *----------------------------------------------------------------*
       5100-GRAVAR-MOV-PASTNSAI SECTION.
      *----------------------------------------------------------------*

           WRITE FD-PASTNSAI-REG  FROM  FD-PASTNMOV-REG.

           MOVE WRK-GRAVACAO TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-PASTNSAI.

           ADD 1
           TO  ACU-GRAVA-PASTNSAI.

      *----------------------------------------------------------------*
       5100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5200-GRAVAR-CAD-PASTNSAI SECTION.
      *----------------------------------------------------------------*

           WRITE FD-PASTNSAI-REG  FROM  FD-PASTNCAD-REG.

           MOVE WRK-GRAVACAO TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-PASTNSAI.

           ADD 1
           TO  ACU-GRAVA-PASTNSAI.

      *----------------------------------------------------------------*
       5200-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA FINALIZAR PROCESSAMENTO.                           *
      *----------------------------------------------------------------*
       7000-FINALIZAR SECTION.
      *----------------------------------------------------------------*

           PERFORM 8100-EMITIR-DISPLAY.

           CLOSE PASTNCAD
                 PASTNMOV
                 PASTNSAI.

           MOVE WRK-FECHAMENTO TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       7000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE EMITIR TOTALIZADORES DO PROCESSAMENTO.               *
      *----------------------------------------------------------------*
       8100-EMITIR-DISPLAY SECTION.
      *----------------------------------------------------------------*

           DISPLAY '**************************************************'.
           DISPLAY '*               PROGRAMA GFCT2478                *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*       PROCESSAMENTO ENCERRADO COM SUCESSO      *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*                                                *'.

           MOVE ACU-LIDOS-PASTNCAD          TO WRK-MASCARA.
           DISPLAY '* REG. LIDOS MESES ANTERIORES.: ' WRK-MASCARA
                                                                  '  *'.
           MOVE ACU-LIDOS-PASTNMOV          TO WRK-MASCARA.
           DISPLAY '* REG. LIDOS DESTE MES........: ' WRK-MASCARA
                                                                  '  *'.
           MOVE ACU-GRAVA-PASTNSAI          TO WRK-MASCARA.
           DISPLAY '* REG. GRAVADOS NA SAIDA......: ' WRK-MASCARA
                                                                  '  *'.
           DISPLAY '**************************************************'.

      *----------------------------------------------------------------*
       8100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE ERROS.                                               *
      *----------------------------------------------------------------*
       9998-ROTINA-ERRO SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2478'
           TO   FRWKGHEA-NOME-PROGRAMA.

           SET  ERRO-ARQUIVO        TO TRUE
           MOVE FRWKGARQ-TAM-LAYOUT TO FRWKGHEA-TAM-DADOS
           MOVE WRK-AREA-FRWKGARQ   TO WRK-BLOCO-INFO-ERRO
                                       (1:FRWKGHEA-TAM-DADOS)

           MOVE 16                  TO RETURN-CODE

           CALL WRK-FRWK2999     USING WRK-AREA-FRWK2999.

           PERFORM 9999-ABENDAR-PROGRAMA

           GOBACK.

      *----------------------------------------------------------------*
       9998-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA ABENDAR O FONTE.                                   *
      *----------------------------------------------------------------*
       9999-ABENDAR-PROGRAMA SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2478'             TO ERR-PGM.

           CALL 'BRAD7100'          USING WRK-BATCH
                                          ERRO-AREA.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*
