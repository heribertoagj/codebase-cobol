      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVEB910.
       AUTHOR.     SIVEK.
      *                                                                *
      *================================================================*
      *                       W I P R O                                *
      *----------------------------------------------------------------*
      *    PROGRAMA....: BVVEB910 - CLONE BVVEA144                     *
      *    PROGRAMADORA: SIVEK                      BRADESCO G439      *
      *    ANALISTA....: SIVEK                      BRADESCO G439      *
      *    DATA........: DEZEMBRO/2022                                 *
      *----------------------------------------------------------------*
      *    OBJETIVO....: TRATA GRANDES VOLUMES                         *
      *                  ATUALIZAR SITUACAO DE OPERACOES ENVIADAS      *
      *                  - RETORNO SITUACAO COM CONTEUDO DO ARQUIVO    *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *     EARQENT   - ARQUIVO DE SOLICITACAO CESSAO       I#BVVEC2  *
      *     SDESPREZ  - ARQUIVO DE SOLICITACAO CESSAO       I#BVVEC4  *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT EARQENT  ASSIGN      TO UT-S-EARQENT
                    FILE STATUS        IS WRK-FS-EARQENT.

           SELECT SDESPREZ ASSIGN      TO UT-S-SDESPREZ
                    FILE STATUS        IS WRK-FS-SDESPREZ.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO EARQENT - OPERACOES PARA GRANDES VOLUMES    *
      *            ORG. SEQUENCIAL - LRECL = 248                       *
      *----------------------------------------------------------------*

       FD  EARQENT
           RECORDING  MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EARQENT             PIC  X(248).

      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO SDESPREZ - DESPREZADOS
      *            ORG. SEQUENCIAL - LRECL = 300                       *
      *----------------------------------------------------------------*

       FD  SDESPREZ
           RECORDING  MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SDESPREZ             PIC  X(300).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVEB910 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

       77  WRK-CKRS0100                PIC X(08)        VALUE
           'CKRS0100'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-MULADORES.
           03  ACU-LID-EARQENT         PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-SDESPREZ        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-SIT-PEND        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-SIT-VOLU        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-SIT-ACAT        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-SIT-RECU        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-SIT-OUTR        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-ATUALIZADOS         PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-ATUALIZ-B054        PIC  9(09) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE OPERACAO DE ARQUIVOS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-EARQENT              PIC X(02)           VALUE SPACES.
       77  WRK-FS-SDESPREZ             PIC X(02)           VALUE SPACES.

       01  WRK-ABERTURA                PIC X(13)           VALUE
           ' NA ABERTURA '.
       01  WRK-FECHAMENTO              PIC X(13)           VALUE
           'NO FECHAMENTO'.
       01  WRK-LEITURA                 PIC X(13)           VALUE
           ' NA LEITURA  '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)      VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)     VALUE 'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA MENSAGENS DE ERRO     *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-ERRO.
          03  FILLER                   PIC X(07)   VALUE SPACES.
          03  FILLER                   PIC X(07)   VALUE
              '* ERRO '.
          03  WRK-OPERACAO             PIC X(13)   VALUE SPACES.
          03  FILLER                   PIC X(12)   VALUE
              ' DO ARQUIVO '.
          03  WRK-NOME-ARQ             PIC X(08)   VALUE SPACES.
          03  FILLER                   PIC X(17)   VALUE
              ' - FILE-STATUS = '.
          03  WRK-FILE-STATUS          PIC X(02)   VALUE SPACES.
          03  FILLER                   PIC X(02)   VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*** AREA PARA ERRO CKRS0100 ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MENSAGEM-01         PIC X(075)          VALUE
               'CKRS0100 - ERRO GRAVE - RETURN <> DE ZEROS E 8'.
           03  WRK-MENSAGEM-02         PIC X(075)          VALUE
               'CKRS0100 - CODIGO DE FUNCAO INCORRETO'.
           03  WRK-MENSAGEM-03         PIC X(075)          VALUE
               'CKRS0100 - JA FOI FEITA UMA CHAMADA EM MODO INITIALIZE'.
           03  WRK-MENSAGEM-04         PIC X(075)          VALUE
               'CKRS0100 - NAO FOI EFETUADA CHAMADA EM MODO INITIALIZE'.
           03  WRK-MENSAGEM-05         PIC X(075)          VALUE
               'CKRS0100 - CAMPO DB2-ID DEVE ESTAR INFORMADO'.
           03  WRK-MENSAGEM-06         PIC X(075)          VALUE
               'CKRS0100 - O PROCESSO NAO SE ENCONTRA CADATRADO'.
           03  WRK-MENSAGEM-07         PIC X(075)          VALUE
               'CKRS0100 - ERRO NAO IDENTIFICADO'.

       01  WRK-LINHA-PROGRAMA.
           03  FILLER                  PIC  X(038)         VALUE
               '************** BVVEB910 **************'.

       01  WRK-LINHA-BRANCO.
           03  FILLER                  PIC  X(038)         VALUE
               '*                                    *'.

       01  WRK-LINHA-RETORNO.
           03  FILLER                  PIC  X(024)         VALUE
               '* CK01-CODIGO-RETORNO : '.
           03  WRK-CODIGO-RETORNO      PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(013)         VALUE
               '           *'.

       01  WRK-LINHA-MENSAGEM.
           03  FILLER                  PIC  X(024)         VALUE
               '* CK01-CODIGO-MENSAGEM: '.
           03  WRK-CODIGO-MENSAGEM     PIC  X(008)         VALUE ZEROS.
           03  FILLER                  PIC  X(008)         VALUE
               '       *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)        VALUE
           '* AREA DO CKRS0100 *'.
      *----------------------------------------------------------------*

       COPY 'I#CKRS01'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)         VALUE
           '* AREA DO ARQUIVO ENTRADA*'.
      *----------------------------------------------------------------*

           COPY  'I#BVVEC2'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)         VALUE
           '* AREA DO ARQUIVO SDESPREZ'.
      *----------------------------------------------------------------*

           COPY  'I#BVVEC4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)          VALUE
           '*  AREA DA POOL7100  *'.
      *----------------------------------------------------------------*

           COPY  'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA DB2  *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE BVVEB055
           END-EXEC.

           EXEC SQL
               INCLUDE BVVEB054
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM DA WORKING BVVEB910    *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 1000-INICIAR-PROGRAMA.

           PERFORM 2000-LER-EARQENT.

           IF  WRK-FS-EARQENT    EQUAL '10'
               DISPLAY   '***********BVVEB910******************* '
               DISPLAY   '*                                    * '
               DISPLAY   '*       ARQ    EARQENT   VAZIO       * '
               DISPLAY   '*                                    * '
               DISPLAY   '************************************** '
           END-IF.

           PERFORM 2100-LER-SDESPREZ.

           IF  WRK-FS-SDESPREZ   EQUAL '10'
               DISPLAY   '***********BVVEB910******************* '
               DISPLAY   '*                                    * '
               DISPLAY   '*       ARQ   SDESPREZ   VAZIO       * '
               DISPLAY   '*                                    * '
               DISPLAY   '************************************** '
           END-IF.

           PERFORM 3000-PROCESSAR.

           PERFORM 4000-FINALIZAR.

           GOBACK.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT  EARQENT SDESPREZ

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.
           PERFORM 1300-TRATAR-CHECKPOINT-RESTART.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-EARQENT.
           PERFORM 1220-TESTAR-FS-SDESPREZ.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-TESTAR-FS-EARQENT          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EARQENT          NOT EQUAL '00'
               MOVE 'EARQENT'          TO WRK-NOME-ARQ
               MOVE WRK-FS-EARQENT     TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-TESTAR-FS-SDESPREZ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SDESPREZ         NOT EQUAL '00'
               MOVE 'SDESPREZ'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SDESPREZ    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-TRATAR-CHECKPOINT-RESTART  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2'                  TO CK01-ID-DB2.
           MOVE 'I'                    TO CK01-FUNCAO.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 1310-TRATAR-ERRO-RESTART
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TRATA ERRO NO ACESSO AO MODULO CKRS0100                     *
      *----------------------------------------------------------------*
       1310-TRATAR-ERRO-RESTART        SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                    TO ERR-TIPO-ACESSO.

           IF  CK01-CODIGO-RETORNO       NOT EQUAL 8
               MOVE WRK-MENSAGEM-01      TO ERR-TEXTO
               MOVE CK01-CODIGO-RETORNO  TO WRK-CODIGO-RETORNO
               MOVE CK01-CODIGO-MENSAGEM TO WRK-CODIGO-MENSAGEM
               DISPLAY WRK-LINHA-PROGRAMA
               DISPLAY WRK-LINHA-BRANCO
               DISPLAY WRK-LINHA-RETORNO
               DISPLAY WRK-LINHA-MENSAGEM
               DISPLAY WRK-LINHA-BRANCO
               DISPLAY WRK-LINHA-PROGRAMA
               PERFORM 9999-ROTINA-ERRO
            END-IF.

            EVALUATE CK01-CODIGO-MENSAGEM
                WHEN 'CKRS0001'
                      MOVE WRK-MENSAGEM-02 TO ERR-TEXTO
                WHEN 'CKRS0002'
                      MOVE WRK-MENSAGEM-03 TO ERR-TEXTO
                WHEN 'CKRS0003'
                      MOVE WRK-MENSAGEM-04 TO ERR-TEXTO
                WHEN 'CKRS0004'
                      MOVE WRK-MENSAGEM-05 TO ERR-TEXTO
                WHEN 'CKRS0005'
                      MOVE WRK-MENSAGEM-06 TO ERR-TEXTO
                WHEN  OTHER
                      MOVE WRK-MENSAGEM-07 TO ERR-TEXTO
                      MOVE CK01-CODIGO-RETORNO  TO WRK-CODIGO-RETORNO
                      MOVE CK01-CODIGO-MENSAGEM TO WRK-CODIGO-MENSAGEM
                      DISPLAY WRK-LINHA-PROGRAMA
                      DISPLAY WRK-LINHA-BRANCO
                      DISPLAY WRK-LINHA-RETORNO
                      DISPLAY WRK-LINHA-MENSAGEM
                      DISPLAY WRK-LINHA-BRANCO
                      DISPLAY WRK-LINHA-PROGRAMA
            END-EVALUATE.

            PERFORM 9999-ROTINA-ERRO.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-LER-EARQENT                SECTION.
      *----------------------------------------------------------------*

           READ EARQENT                INTO BVVEC2-BVVEB055

           IF  WRK-FS-EARQENT          EQUAL '10'
               GO TO 2000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-EARQENT.

           ADD     1                   TO ACU-LID-EARQENT.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-SDESPREZ               SECTION.
      *----------------------------------------------------------------*

           READ SDESPREZ               INTO REG-BVVEC4

           IF  WRK-FS-SDESPREZ         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1220-TESTAR-FS-SDESPREZ

           ADD     1                   TO ACU-LID-SDESPREZ.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM UNTIL WRK-FS-EARQENT  EQUAL '10'

AQUI           EVALUATE BVVEC2-CSIT-OPER-CIPAG

                   WHEN 'P'
                      ADD  1           TO ACU-LID-SIT-PEND
                   WHEN 'V'
                      ADD  1           TO ACU-LID-SIT-VOLU
                   WHEN 'A'
                      ADD  1           TO ACU-LID-SIT-ACAT
                   WHEN 'R'
                      ADD  1           TO ACU-LID-SIT-RECU
                   WHEN  OTHER
                      ADD  1           TO ACU-LID-SIT-OUTR

               END-EVALUATE

AQUI           IF  BVVEC2-CSIT-OPER-CIPAG NOT EQUAL 'P' AND 'V'
                   PERFORM 3100-ATUALIZAR-BVVEB055
                   PERFORM 3410-TRATAR-RESTART
               END-IF

               PERFORM 2000-LER-EARQENT
           END-PERFORM.

           PERFORM UNTIL WRK-FS-SDESPREZ EQUAL '10'
               PERFORM 3200-ATUALIZAR-BVVEB055
               PERFORM 3300-ATUALIZAR-BVVEB054
               PERFORM 3410-TRATAR-RESTART
               PERFORM 2100-LER-SDESPREZ
           END-PERFORM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-ATUALIZAR-BVVEB055         SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEC2-CPROT-SIST-ORIGE TO CPROT-SIST-ORIGE
                                        OF BVVEB055
AQUI       MOVE BVVEC2-CSIT-OPER-CIPAG  TO CSIT-OPER-CIPAG
                                        OF BVVEB055

           EXEC SQL
             UPDATE DB2PRD.TOPER_AGNDA_CIPAG
             SET    CSIT_OPER_CIPAG  = :BVVEB055.CSIT-OPER-CIPAG
             WHERE  CPROT_SIST_ORIGE = :BVVEB055.CPROT-SIST-ORIGE
           END-EXEC.


           IF (SQLCODE             NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'BVVEB055'         TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.
           ADD 1                       TO ACU-ATUALIZADOS
           .
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-ATUALIZAR-BVVEB055         SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEC4-PROTOCOLO        TO CPROT-SIST-ORIGE
                                        OF BVVEB055
           MOVE 'R'                     TO CSIT-OPER-CIPAG
                                        OF BVVEB055
           MOVE 'CNPJ TITULAR NAO ENCOTRADO'
                                        TO IMOTVO-REJEI-CIPAG
                                        OF BVVEB055

           EXEC SQL
             UPDATE DB2PRD.TOPER_AGNDA_CIPAG
             SET    CSIT_OPER_CIPAG    = :BVVEB055.CSIT-OPER-CIPAG,
                    IMOTVO_REJEI_CIPAG = :BVVEB055.IMOTVO-REJEI-CIPAG
             WHERE  CPROT_SIST_ORIGE   = :BVVEB055.CPROT-SIST-ORIGE
           END-EXEC.

           IF (SQLCODE             NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'BVVEB055'         TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.
           ADD 1                       TO ACU-ATUALIZADOS
           .
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-ATUALIZAR-BVVEB054         SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEC4-PROTOCOLO        TO CPROT-SIST-ORIGE
                                        OF BVVEB054

           EXEC SQL
             UPDATE DB2PRD.TCTRL_MSGRA_CIPAG
             SET    CSIT_PROCM_CIPAG= '4'
             WHERE  CPROT_SIST_ORIGE   = :BVVEB054.CPROT-SIST-ORIGE
           END-EXEC.

           IF (SQLCODE             NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'BVVEB054'         TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.
           ADD 1                       TO ACU-ATUALIZ-B054
           .
      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3410-TRATAR-RESTART             SECTION.
      *---------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2'                  TO CK01-ID-DB2.
           MOVE 'P'                    TO CK01-FUNCAO.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 1310-TRATAR-ERRO-RESTART
           END-IF.

      *---------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           PERFORM 4200-FIM-CHECKPOINT-RESTART.

           CLOSE EARQENT SDESPREZ

           MOVE WRK-FECHAMENTO         TO  WRK-OPERACAO.
           PERFORM 1200-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** BVVEB910 ******************'
           DISPLAY '*                                         *'

           MOVE ACU-LID-EARQENT       TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS EARQENT: ' WRK-MASCARA '       *'

           MOVE ACU-LID-SIT-PEND      TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS PENDENT: ' WRK-MASCARA '       *'

           MOVE ACU-LID-SIT-VOLU      TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS VOLUME : ' WRK-MASCARA '       *'

           DISPLAY '*                                         *'

           MOVE ACU-LID-SIT-ACAT      TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS ACATADO: ' WRK-MASCARA '       *'

           MOVE ACU-LID-SIT-RECU      TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS RECUSAD: ' WRK-MASCARA '       *'

           MOVE ACU-LID-SIT-OUTR      TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS OUTROS : ' WRK-MASCARA '       *'

           MOVE ACU-ATUALIZADOS       TO WRK-MASCARA.
           DISPLAY '*   REG. ATUAL B055   : ' WRK-MASCARA '       *'

           MOVE ACU-ATUALIZ-B054      TO WRK-MASCARA.
           DISPLAY '*   REG. ATUAL B054   : ' WRK-MASCARA '       *'

           DISPLAY '*                                         *'
           DISPLAY '*************** BVVEB910 ******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZAR CHECK POINT                                      *
      *----------------------------------------------------------------*
       4200-FIM-CHECKPOINT-RESTART     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'F'                    TO CK01-FUNCAO.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 1310-TRATAR-ERRO-RESTART
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVEB910'             TO ERR-PGM.

           CALL 'BRAD7100'         USING WRK-BATCH
                                         ERRO-AREA
                                         SQLCA.

           GOBACK.
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
