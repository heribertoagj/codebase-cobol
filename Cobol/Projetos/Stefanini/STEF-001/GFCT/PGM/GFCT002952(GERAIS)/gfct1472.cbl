      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1472.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT1472                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: UBIRAJARA (BIRA)                         *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........: HELENA                                   *
      *                                                                *
      *    DATA.............: DEZ/2010                                 *
      *                                                                *
      *    PROJETO..........: PWT-10-3168 FICHA DE CADASTRO PJ         *
      *                                                                *
      *    OBJETIVO      :                                             *
      *      INICIALIZAR CAMPOS NOVOS CRIADOS NO BOOK I#GFCTIL.        *
      *                                                                *
      *    ARQUIVOS      :                                             *
      *      DDNAME                               INCLUDE/BOOK         *
      *      ENTRADA                                I#GFCTIL           *
      *      SAIDA                                  I#GFCTIL           *
      *                                                                *
      *================================================================*

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

           SELECT ENTRADA  ASSIGN      TO UT-S-ENTRADA
                      FILE STATUS      IS WRK-FS-ENTRADA.

           SELECT SAIDA    ASSIGN      TO UT-S-SAIDA
                      FILE STATUS      IS WRK-FS-SAIDA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE REGISTROS ENVIADOS PARA COBRANCA NO MORA *
      *             ORG. SEQUENTIAL    -    LRECL = 0300               *
      *----------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE REGISTROS ENVIADOS PARA COBRANCA NO MORA *
      *             ORG. SEQUENTIAL    -    LRECL = 0300               *
      *----------------------------------------------------------------*

       FD  SAIDA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* INICIO DA WORKING GFCT1472   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      ACUMULADORES            *'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS                   PIC 9(09) COMP-3    VALUE ZEROS.
       01  ACU-ALTERADOS               PIC 9(09) COMP-3    VALUE ZEROS.
       01  ACU-GRAVADOS                PIC 9(09) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREAS AUXILIARES        *'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC X(08)           VALUE
           'BATCH'.
       01  WRK-MASCARA                 PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA FILE STATUS          *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ENTRADA              PIC X(02)           VALUE SPACES.
       01  WRK-FS-SAIDA                PIC X(02)           VALUE SPACES.

       01  WRK-ABERTURA                PIC X(13)           VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC X(13)           VALUE
           'NA LEITURA'.
       01  WRK-GRAVACAO                PIC X(13)           VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC X(13)           VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA MENSAGEM DE ERRO      *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-ERRO.
           03  FILLER                  PIC X(07)           VALUE SPACES.
           03  FILLER                  PIC X(07)           VALUE
               '* ERRO '.
           03  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           03  FILLER                  PIC X(12)           VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC X(08)           VALUE SPACES.
           03  FILLER                  PIC X(17)           VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC X(02)           VALUE SPACES.
           03  FILLER                  PIC X(02)           VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA POOL7100      *'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT1472   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-0-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-0-INICIAR-PROGRAMA.

           PERFORM 2000-0-VERIFICAR-VAZIO.

           PERFORM 3000-0-PROCESSAR-DADOS UNTIL
                       WRK-FS-ENTRADA  EQUAL '10'.

           PERFORM 4000-0-FINALIZAR-PROGRAMA.

           STOP RUN.

      *----------------------------------------------------------------*
       0000-0-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-0-INICIAR-PROGRAMA          SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ENTRADA
                OUTPUT SAIDA.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-0-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-0-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-0-TESTAR-FILE-STATUS        SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-0-TESTAR-FS-ENTRADA.

           PERFORM 1120-0-TESTAR-FS-SAIDA.

      *----------------------------------------------------------------*
       1100-0-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-0-TESTAR-FS-ENTRADA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL ZEROS
               MOVE 'ENTRADA '         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ENTRADA     TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               PERFORM 9999-9-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-0-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-0-TESTAR-FS-SAIDA           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIDA            NOT EQUAL ZEROS
               MOVE 'SAIDA  '          TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-SAIDA       TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               PERFORM 9999-9-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-0-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-0-VERIFICAR-VAZIO           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-0-LER-ENTRADA.

           IF  WRK-FS-ENTRADA          EQUAL '10'
               DISPLAY '***************************************'
               DISPLAY '*            GFCT1472                 *'
               DISPLAY '*      ARQUIVO ENTRADA  VAZIO         *'
               DISPLAY '***************************************'
               PERFORM 4000-0-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       2000-0-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-0-LER-ENTRADA               SECTION.
      *----------------------------------------------------------------*

           READ ENTRADA.

           IF  WRK-FS-ENTRADA          EQUAL '10'
               GO                      TO 2100-0-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-0-TESTAR-FS-ENTRADA.

           ADD 1                       TO ACU-LIDOS.

      *----------------------------------------------------------------*
       2100-0-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-0-PROCESSAR-DADOS           SECTION.
      *----------------------------------------------------------------*

           MOVE  REG-COBRANCA OF ENTRADA  TO
                                 REG-COBRANCA OF SAIDA.

           IF   COBR-NUM-PARC OF SAIDA   NOT  NUMERIC
                MOVE  ZEROS  TO  COBR-NUM-PARC OF SAIDA
                ADD   1      TO  ACU-ALTERADOS
           END-IF.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-COBRANCA OF SAIDA.

           PERFORM 1120-0-TESTAR-FS-SAIDA.

           ADD 1                       TO ACU-GRAVADOS.

           PERFORM 2100-0-LER-ENTRADA.

      *----------------------------------------------------------------*
       3000-0-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-0-FINALIZAR-PROGRAMA        SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-0-EMITIR-DISPLAY.

           CLOSE ENTRADA
                 SAIDA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-0-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       4000-0-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-0-EMITIR-DISPLAY            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*********************** GFCT1472 '
                   '***********************'.
           DISPLAY '                                 '
                   '                      *'.
           MOVE ACU-LIDOS              TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGISTROS LIDOS    EM ENTRADA:  '
              WRK-MASCARA ' *'.

           MOVE ACU-ALTERADOS          TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. COM PARCELA INICIALIZADA: '
              WRK-MASCARA ' *'.

           MOVE ACU-GRAVADOS           TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGISTROS GRAVADOS EM SAIDA   : '
              WRK-MASCARA ' *'.
           DISPLAY '                                 '
                   '                      *'.
           DISPLAY '*********************** GFCT1472 '
                   '***********************'.

      *----------------------------------------------------------------*
       4100-0-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-9-ROTINA-ERRO               SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1472'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           MOVE 4                      TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-9-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
