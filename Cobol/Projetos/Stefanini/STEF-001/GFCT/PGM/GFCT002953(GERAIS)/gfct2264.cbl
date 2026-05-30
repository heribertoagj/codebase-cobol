      *================================================================*
       IDENTIFICATION DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2264.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *             W I P R O  -  T E C H N O L O G I E S              *
      *----------------------------------------------------------------*
      *    CENTRO CUSTO:   GFCT - GESTAO FLEX. COBRANCA DE TARIFAS     *
      *    PROJETO.....:   ARQUIVO PARA EXTRATO                        *
      *    PROGRAMA....:   GFCT2264                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA                 *
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA                 *
      *    LINGUAGEM...:   BATCH                                       *
      *    DATA........:   21/02/2017                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   BALANCE LINE ENTRE EXTRATO E ARQUIVO DO     *
      *                    CLIE, PESSOA FISICA.                        *
      *----------------------------------------------------------------*
      *    MODULOS:                                                    *
      *    FRWK2999        MODULO FUNCIONAL PARA TRAT. DE ERROS (GLOG) *
      *    BRAD7100        MODULO FUNCIONAL PARA TRAT. DE ERROS EVT    *
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

           SELECT EXTRAENT    ASSIGN TO UT-S-EXTRAENT
           FILE STATUS               IS WRK-FS-EXTRAENT.

           SELECT CLIEFISI    ASSIGN TO UT-S-CLIEFISI
           FILE STATUS               IS WRK-FS-CLIEFISI.

           SELECT EXTRASAI    ASSIGN TO UT-S-EXTRASAI
           FILE STATUS               IS WRK-FS-EXTRASAI.

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

       FD  EXTRAENT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWEXX'.

      *----------------------------------------------------------------*
      *  OUTPUT...: LAYOUT DO ARQUIVO                                  *
      *             ORG. SEQUENCIAL                                    *
      *----------------------------------------------------------------*

       FD  CLIEFISI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#CLIE20'.

      *----------------------------------------------------------------*
      *  OUTPUT...: LAYOUT DO ARQUIVO                                  *
      *             ORG. SEQUENCIAL                                    *
      *----------------------------------------------------------------*

       FD  EXTRASAI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWEXX'.

      *================================================================*
       WORKING-STORAGE SECTION.
      *================================================================*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           'INICIO DA WORKING STORAGE GFCT2264'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032) VALUE
           'AREAS AUXILIARES'.
      *----------------------------------------------------------------*

       01  WRK-FRWK2999                PIC  X(008)         VALUE
           'FRWK2999'.
       01  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-EXTRAENT-FIM            PIC  X(001)         VALUE SPACES.

       01  WRK-CONTA-11                PIC  9(011)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CONTA-11.
           03  FILLER                  PIC  X(004).
           03  WRK-CONTA-7             PIC  9(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'AREAS CHAVES BALANCE LINE'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-EXTRAENT.
        05  WRK-GFCT-AGENCIA           PIC  9(005)         VALUE ZEROS.
        05  WRK-GFCT-CONTA             PIC  9(007)         VALUE ZEROS.

       01  WRK-CHAVE-CLIEFISI.
        05  WRK-CLIE-AGENCIA           PIC  9(005)         VALUE ZEROS.
        05  WRK-CLIE-CONTA             PIC  9(007)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'AREAS ACUMULADORES'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS-EXTRAENT          PIC  9(009)  COMP-3  VALUE ZEROS.
       01  ACU-LIDOS-CLIEFISI          PIC  9(009)  COMP-3  VALUE ZEROS.
       01  ACU-GRAVA-EXTRASAI          PIC  9(009)  COMP-3  VALUE ZEROS.
       01  ACU-DESPREZADOS             PIC  9(009)  COMP-3  VALUE ZEROS.

       01  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9     VALUE ZEROS.

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

       01  WRK-FS-EXTRAENT            PIC  X(002) VALUE SPACES.
       01  WRK-FS-CLIEFISI            PIC  X(002) VALUE SPACES.
       01  WRK-FS-EXTRASAI            PIC  X(002) VALUE SPACES.

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
           'AREA DA COPYBOOK DA BRAD7600'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'FIM DA WORKING GFCT2264'.
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
                   WRK-EXTRAENT-FIM EQUAL 'S'.

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

           OPEN INPUT  EXTRAENT
                       CLIEFISI
                OUTPUT EXTRASAI.

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

           PERFORM 1110-TESTAR-FS-EXTRAENT.

           PERFORM 1120-TESTAR-FS-CLIEFISI.

           PERFORM 1130-TESTAR-FS-EXTRASAI.

      *----------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TESTAR FILE-STATUS DO ARQUIVO.                     *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EXTRAENT SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRAENT NOT EQUAL '00'
               DISPLAY '************** GFCT2264 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              EXTRAENT             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-EXTRAENT
                                                  '         *'
               DISPLAY '************** GFCT2264 *************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE 'EXTRAENT'          TO WRK-NOME-ARQUIVO
                                           FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-EXTRAENT     TO WRK-FS
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
       1120-TESTAR-FS-CLIEFISI SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEFISI NOT EQUAL '00'
               DISPLAY '************** GFCT2264 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              CLIEFISI             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-CLIEFISI
                                                  '         *'
               DISPLAY '************** GFCT2264 *************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE 'CLIEFISI'          TO WRK-NOME-ARQUIVO
                                           FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-CLIEFISI     TO WRK-FS
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
       1130-TESTAR-FS-EXTRASAI SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRASAI NOT EQUAL '00'
               DISPLAY '************** GFCT2264 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              EXTRASAI             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-EXTRASAI
                                                  '         *'
               DISPLAY '************** GFCT2264 *************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE 'EXTRASAI'          TO WRK-NOME-ARQUIVO
                                           FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-EXTRASAI     TO WRK-FS
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

           PERFORM 2100-LER-EXTRAENT.

           IF  ACU-LIDOS-EXTRAENT EQUAL ZEROS
               DISPLAY  '********* GFCT2264 *********'
               DISPLAY  '*     ARQUIVO  EXTRAENT    *'
               DISPLAY  '*          VAZIO.          *'
               DISPLAY  '********* GFCT2264 *********'
           END-IF.

           PERFORM 2200-LER-CLIEFISI.

           IF  ACU-LIDOS-CLIEFISI EQUAL ZEROS
               DISPLAY  '********* GFCT2264 *********'
               DISPLAY  '*     ARQUIVO  EXTRAENT    *'
               DISPLAY  '*          VAZIO.          *'
               DISPLAY  '********* GFCT2264 *********'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA LER ARQUIVO.                                       *
      *----------------------------------------------------------------*
       2100-LER-EXTRAENT SECTION.
      *----------------------------------------------------------------*

           READ EXTRAENT.

           IF  WRK-FS-EXTRAENT    EQUAL '10'
               MOVE 'S'           TO WRK-EXTRAENT-FIM
               MOVE HIGH-VALUES   TO WRK-CHAVE-EXTRAENT
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA       TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-EXTRAENT.

           MOVE GFCTWEXX-NR-AG    OF EXTRAENT
                                  TO WRK-GFCT-AGENCIA.

           MOVE GFCTWEXX-NR-CONTA OF EXTRAENT
                                  TO WRK-CONTA-11
           MOVE WRK-CONTA-7       TO WRK-GFCT-CONTA.

           ADD 1                  TO ACU-LIDOS-EXTRAENT.

      *----------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA LER ARQUIVO.                                       *
      *----------------------------------------------------------------*
       2200-LER-CLIEFISI SECTION.
      *----------------------------------------------------------------*

           READ CLIEFISI.

           IF  WRK-FS-CLIEFISI  EQUAL '10'
               MOVE HIGH-VALUES TO WRK-CHAVE-CLIEFISI
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA     TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-CLIEFISI.

           MOVE CAD-AGENCIA     TO WRK-CLIE-AGENCIA.

           MOVE CAD-CONTA       TO WRK-CLIE-CONTA.

           ADD 1                TO ACU-LIDOS-CLIEFISI.

      *----------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO.                                       *
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-EXTRAENT     EQUAL WRK-CHAVE-CLIEFISI
               PERFORM 5100-GRAVAR-SAIDA-EXTRASAI
               PERFORM 2100-LER-EXTRAENT
           ELSE
               IF  WRK-CHAVE-EXTRAENT LESS WRK-CHAVE-CLIEFISI
                   ADD 1              TO   ACU-DESPREZADOS
                   PERFORM 2100-LER-EXTRAENT
               ELSE
                   PERFORM 2200-LER-CLIEFISI
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA GRAVACAO DE ARQUIVO.                               *
      *----------------------------------------------------------------*
       5100-GRAVAR-SAIDA-EXTRASAI SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWEXX-DETALHES
           OF   EXTRAENT
           TO   GFCTWEXX-DETALHES
           OF   EXTRASAI.

           WRITE GFCTWEXX-DETALHES OF EXTRASAI.

           MOVE WRK-GRAVACAO       TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-EXTRASAI.

           ADD 1                   TO ACU-GRAVA-EXTRASAI.

      *----------------------------------------------------------------*
       5100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA FINALIZAR PROCESSAMENTO.                           *
      *----------------------------------------------------------------*
       7000-FINALIZAR SECTION.
      *----------------------------------------------------------------*

           PERFORM 8100-EMITIR-DISPLAY.

           CLOSE EXTRAENT
                 CLIEFISI
                 EXTRASAI.

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
           DISPLAY '*               PROGRAMA GFCT2264                *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*       PROCESSAMENTO ENCERRADO COM SUCESSO      *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*                                                *'.

           MOVE ACU-LIDOS-EXTRAENT          TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS EXTRATO.....:  'WRK-MASCARA
                                                               '     *'.
           MOVE ACU-LIDOS-CLIEFISI          TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS CLIE PF.....:  'WRK-MASCARA
                                                               '     *'.
           MOVE ACU-GRAVA-EXTRASAI          TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS EXTRATO..:  'WRK-MASCARA
                                                               '     *'.
           MOVE ACU-DESPREZADOS             TO WRK-MASCARA.
           DISPLAY '* REGISTROS DESPREZADOS.......:  'WRK-MASCARA
                                                               '     *'.
           DISPLAY '**************************************************'.

      *----------------------------------------------------------------*
       8100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE ERROS.                                               *
      *----------------------------------------------------------------*
       9998-ROTINA-ERRO SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2264'
           TO   FRWKGHEA-NOME-PROGRAMA.

           SET  ERRO-ARQUIVO        TO TRUE
           MOVE FRWKGARQ-TAM-LAYOUT TO FRWKGHEA-TAM-DADOS
           MOVE WRK-AREA-FRWKGARQ   TO WRK-BLOCO-INFO-ERRO
                                       (1:FRWKGHEA-TAM-DADOS)

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

           MOVE 'GFCT2264'             TO ERR-PGM.

           CALL 'BRAD7100'          USING WRK-BATCH
                                          ERRO-AREA.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*
