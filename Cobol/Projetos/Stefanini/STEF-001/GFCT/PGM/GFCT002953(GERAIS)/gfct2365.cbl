      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2365.
       AUTHOR.     FRANCISCO LOURENCO.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2365                                    *
      *    PROGRAMADOR.:   FRANCISCO LOURENCO      - PROCWORK /GP.50   *
      *    ANALISTA....:   MARIA VALERIA TORQUATO  - PROCWORK /GP.50   *
      *    DATA........:   17/03/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:                                               *
      *                 ATUALIZA A TABELA DE MOVIMENTO DE EVENTOS DE   *
      *                 CORRENTISTA (GFCTB092-DB2PRD.MOVTO_EVNTO_CRRTT)*
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                       ARQIMG92                   I#GFCTWR      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                       DB2PRD.MOVTO_EVNTO_CRRTT   GFCTB092      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA CHAMADA POOL7100 - CANCELAMENTO.            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5555 - MODULO PARA UPDATE DE SITUACOES NAS TABELAS      *
      *    CKRS0100 - TRATAMENTO CHECKPOINT RESTART DB2                *
      *    CKRS1000 - CONEXAO COM DB2                                  *
      *    POOL7100 - MODULO TRATAMENTO DE ERRO                        *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       INPUT-OUTPUT                    SECTION.
      *================================================================*
       FILE-CONTROL.

           SELECT ARQIMG92 ASSIGN      TO UT-S-ARQIMG92
                      FILE STATUS      IS WRK-FS-ARQIMG92.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*


      *----------------------------------------------------------------*
      *   INPUT:  REGISTROS EVENTO LIBERADO E ARQIMG92S                *
      *           ORG.SEQUENCIAL  -  LRECL = 073                       *
      *----------------------------------------------------------------*

       FD  ARQIMG92
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTWR.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*   INICIO DA WORKING GFCT2365 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-ARQIMG92       PIC  9(009) COMP-3  VALUE ZEROS.
           05 ACU-ATUALZ-GFCTB092      PIC  9(009) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC  X(008)         VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC  Z.ZZZ.ZZ9      VALUE ZEROS.
           05 WRK-MODULO               PIC  X(008)         VALUE SPACES.
           05 WRK-FIM-ARQIMG92         PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
      *                 CONTROLE DE CHECKPOINT/RESTART                 *
      *----------------------------------------------------------------*

       01  FILLER                      PIC  X(050) VALUE
           '* CONTROLE DE CHECKPOINT/RESTART *'.

       01  WRK-CKRS0100                PIC  X(008)         VALUE
           'CKRS0100'.

       COPY 'I#CKRS01'.

       01  WRK-AREA-RESTART.
           05  WRK-CKP-LIDOS-ARQIMG92  PIC  9(009)         VALUE ZEROS.
           05  WRK-CKP-ATUALZ-GFCTB092 PIC  9(009)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  AREA TO FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ARQIMG92             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01 WRK-LEITURA                  PIC  X(013)         VALUE
           ' NA  LEITURA '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  MENSAGENS DE ERRO           *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(007)         VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE ' *'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* AREA PARA GFCT5555 *'.
      *---------------------------------------------------------------*

       01  WRK-5555-AREA-ENTRADA.
           05  WRK-5555-CHAMADOR       PIC  X(008)         VALUE SPACES.
           05  WRK-5555-DENVIO-MOVTO   PIC  X(010)         VALUE SPACES.
           05  WRK-5555-CROTNA-ORIGE   PIC  X(004)         VALUE SPACES.
           05  WRK-5555-CNRO-ARQ       PIC  9(002)         VALUE ZEROS.
           05  WRK-5555-CSEQ-MOVTO     PIC  9(011)         VALUE ZEROS.
           05  WRK-5555-CSIT-EVNTO     PIC  9(002)         VALUE ZEROS.
           05  WRK-5555-CORRENTISTA    PIC  X(001)         VALUE SPACES.

       01  WRK-5555-AREA-SAIDA.
           05  WRK-5555-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5555-COD-SQL-ERRO   PIC S9(003)         VALUE ZEROS.
           05  WRK-5555-DESC-MSG       PIC  X(070)         VALUE SPACES.

       01  IO-PCB.
           05  IO-LTERM                PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  IO-STATUS               PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE SPACES.
           05  IO-MODNAME              PIC  X(008)         VALUE SPACES.

       01  ALT-PCB.
           05  ALT-LTERM               PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  ALT-STATUS              PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE SPACES.
           05  ALT-MODNAME             PIC  X(008)         VALUE SPACES.

       COPY I#GFCT0M.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*       AREA TO POOL7100*'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA DE TABELAS DB2         *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    CONTROLE GERAL DE PROCESSOS                                 *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR-DADOS
             UNTIL WRK-FIM-ARQIMG92    EQUAL 'S'.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ABRIR ARQUIVOS                                              *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-INICIALIZAR-CHECKPOINT.

           OPEN INPUT  ARQIMG92.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 9200-TESTAR-FILE-STATUS.

           PERFORM 1200-VERIFICAR-VAZIO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR CONEXAO COM DB2                          *
      *----------------------------------------------------------------*
       1100-INICIALIZAR-CHECKPOINT     SECTION.
      *----------------------------------------------------------------*

      *--- INICIALIZA CHECKPOINT RESTART DB2

           MOVE SPACES                 TO CK01-PLAN.
           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'I'                    TO CK01-FUNCAO.

           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
           MOVE LENGTH OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.

           PERFORM 9300-ROTINA-CHECKPONT-RESTART.

           IF  CK01-RESTART
               MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
                                       TO WRK-AREA-RESTART
               MOVE WRK-CKP-LIDOS-ARQIMG92
                                       TO ACU-LIDOS-ARQIMG92
               MOVE WRK-CKP-ATUALZ-GFCTB092
                                       TO ACU-ATUALZ-GFCTB092
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA VERIFICAR ARQUIVOS VAZIOS                            *
      *----------------------------------------------------------------*
       1200-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ARQIMG92.

           IF WRK-FS-ARQIMG92          EQUAL '10'
               DISPLAY '*************** GFCT2365 **************'
               DISPLAY '*                                     *'
               DISPLAY '*       ARQUIVO ARQIMG92 VAZIO        *'
               DISPLAY '*                                     *'
               DISPLAY '*       TERMINO NORMAL                *'
               DISPLAY '*                                     *'
               DISPLAY '*************** GFCT2365 **************'
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ATUALIZACAO TABELA GFCTB092                                *
      *----------------------------------------------------------------*
       2000-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-ATUALIZAR-GFCTB092.

           PERFORM 2300-ACESSAR-GFCT5555.

      *--- PROCESSA CHECKPOINT RESTART DB2

           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'P'                    TO CK01-FUNCAO.

           MOVE ACU-LIDOS-ARQIMG92     TO WRK-CKP-LIDOS-ARQIMG92.
           MOVE ACU-ATUALZ-GFCTB092    TO WRK-CKP-ATUALZ-GFCTB092.

           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
           MOVE LENGTH OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.

           PERFORM 9300-ROTINA-CHECKPONT-RESTART.

           PERFORM 2100-LER-ARQIMG92.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA LER ARQUIVO ARQIMG92                                 *
      *----------------------------------------------------------------*
       2100-LER-ARQIMG92               SECTION.
      *----------------------------------------------------------------*

           READ ARQIMG92.

           IF  WRK-FS-ARQIMG92         EQUAL '10'
               MOVE 'S'                TO WRK-FIM-ARQIMG92

               GO                      TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 9210-TESTAR-FS-ARQIMG92.

           ADD 1                       TO ACU-LIDOS-ARQIMG92.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ATUALIZACAO TABELA GFCTB092                                *
      *----------------------------------------------------------------*
       2200-ATUALIZAR-GFCTB092         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWR-CROTNA-ORIGE    TO CROTNA-ORIGE-MOVTO
                                                            OF GFCTB092.
           MOVE GFCTWR-DENVIO-MOVTO    TO DENVIO-MOVTO-TARIF
                                                            OF GFCTB092.
           MOVE GFCTWR-CNRO-ARQ        TO CNRO-ARQ-MOVTO    OF GFCTB092.
           MOVE GFCTWR-CSEQ-MOVTO      TO CSEQ-MOVTO        OF GFCTB092.

           MOVE 16                     TO CSIT-EVNTO-RECBD  OF GFCTB092.

           PERFORM 2210-ALTERAR-GFCTB092.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ALTERAR TABELA GFCTB092                                    *
      *----------------------------------------------------------------*
       2210-ALTERAR-GFCTB092           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             UPDATE DB2PRD.MOVTO_EVNTO_CRRTT
               SET
                   CSIT_EVNTO_RECBD     = :GFCTB092.CSIT-EVNTO-RECBD
             WHERE
                   DENVIO_MOVTO_TARIF   = :GFCTB092.DENVIO-MOVTO-TARIF
             AND   CROTNA_ORIGE_MOVTO   = :GFCTB092.CROTNA-ORIGE-MOVTO
             AND   CNRO_ARQ_MOVTO       = :GFCTB092.CNRO-ARQ-MOVTO
             AND   CSEQ_MOVTO           = :GFCTB092.CSEQ-MOVTO
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'MOVTO_EVNTO_CRRTT '
                                       TO ERR-DBD-TAB
                MOVE 'UPDATE    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM

                PERFORM 9999-ROTINA-ERRO
           END-IF.

           ADD 1                       TO ACU-ATUALZ-GFCTB092.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * REALIZA ACESSO AO MODULO GFCT5555
      ******************************************************************
      *-----------------------------------------------------------
       2300-ACESSAR-GFCT5555           SECTION.
      *-----------------------------------------------------------

           MOVE 'H'                        TO WRK-5555-CORRENTISTA.
           MOVE 16                         TO WRK-5555-CSIT-EVNTO
           MOVE GFCTWR-DENVIO-MOVTO        TO WRK-5555-DENVIO-MOVTO.
           MOVE GFCTWR-CROTNA-ORIGE        TO WRK-5555-CROTNA-ORIGE.
           MOVE GFCTWR-CNRO-ARQ            TO WRK-5555-CNRO-ARQ.
           MOVE GFCTWR-CSEQ-MOVTO          TO WRK-5555-CSEQ-MOVTO.
           MOVE 'GFCT2365'                 TO WRK-5555-CHAMADOR.

           MOVE 'GFCT5555'                 TO WRK-MODULO.

           CALL WRK-MODULO             USING  WRK-5555-AREA-ENTRADA
                                              WRK-5555-AREA-SAIDA
                                              GFCT0M-AREA-ERROS
                                              IO-PCB
                                              ALT-PCB.

           IF  WRK-5555-COD-RETORNO        NOT EQUAL ZEROS
               IF  WRK-5555-COD-RETORNO    EQUAL 99
                   MOVE WRK-5555-COD-SQL-ERRO
                                           TO WRK-MASCARA

                   DISPLAY '******************************** GFCT2365 **
      -                    '******************************'
                   DISPLAY '*
      -                    '                             *'
                   DISPLAY '* ERRO NO MODULO GFCT5555
      -                    '                             *'
                   DISPLAY '*
      -                    '                             *'
                   DISPLAY '* SQLCODE: ' WRK-MASCARA '
      -                    '                                 *'
                   DISPLAY '*
      -                    '                             *'
                   DISPLAY '* ' WRK-5555-DESC-MSG ' *'
                   DISPLAY '*
      -                   '                             *'
                   DISPLAY '******************************** GFCT2365 **
      -                    '******************************'
               ELSE
                   MOVE 'APL'              TO ERR-TIPO-ACESSO
                   MOVE WRK-5555-DESC-MSG  TO ERR-TEXTO
               END-IF

               MOVE 'GFCT5555'             TO ERR-MODULO

               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS FINAIS                                       *
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 9100-EMITIR-ESTATISTICA.

           CLOSE ARQIMG92.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 9200-TESTAR-FILE-STATUS.

      *--- FINALIZA CHECKPOINT RESTART DB2

           MOVE 'F'                    TO CK01-FUNCAO.
           MOVE 'DB2'                  TO CK01-ID-DB2.

           PERFORM 9300-ROTINA-CHECKPONT-RESTART.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ESTATISTICAS DE PROCESSAMENTO                               *
      *----------------------------------------------------------------*
       9100-EMITIR-ESTATISTICA         SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-ARQIMG92     TO WRK-MASCARA.

           DISPLAY '******************** GFCT2365 '
                   '********************'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '*        PROGRAMA ENCERRADO CO'
                   'M SUCESSO          *'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '* ============================'
                   '=====              *'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '*   TOTAL DE LIDOS EM ARQIMG92......: '
                   WRK-MASCARA '  *'.

           MOVE ACU-ATUALZ-GFCTB092    TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE ATUALIZADOS GFCTB092...: '
                   WRK-MASCARA '  *'.

           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '*                   TERMINO NO'
                   'RMAL               *'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '******************** GFCT2365 '
                   '********************'.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DOS ARQUIVOS                      *
      *----------------------------------------------------------------*
       9200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 9210-TESTAR-FS-ARQIMG92.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO ARQIMG92               *
      *----------------------------------------------------------------*
       9210-TESTAR-FS-ARQIMG92         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQIMG92          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'ARQIMG92'          TO WRK-NOME-ARQ
              MOVE WRK-FS-ARQIMG92     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *            ROTINA DE TRATAMENTO DE CHECKPOINT/RESTART          *
      *----------------------------------------------------------------*
       9300-ROTINA-CHECKPONT-RESTART   SECTION.
      *----------------------------------------------------------------*

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 9310-TRATAR-ERRO-RESTART

               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR ERRO NO ACESSO AO MODULO CKRS0100        *
      *----------------------------------------------------------------*
       9310-TRATAR-ERRO-RESTART        SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           IF  CK01-CODIGO-RETORNO     EQUAL 8
               EVALUATE CK01-CODIGO-MENSAGEM
                   WHEN 'CKRS0001'
                        MOVE 'CKRS0100 - CODIGO DE FUNCAO INCORRETO'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0002'
                        MOVE 'CKRS0100 - JA FOI FEITA UMA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0003'
                        MOVE 'CKRS0100 - NAO FOI EFETUADA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0004'
                        MOVE 'CKRS0100 - CAMPO DB2-ID DEVE ESTAR INFORMA
      -                      'DO'      TO ERR-TEXTO

                   WHEN 'CKRS0005'
                        MOVE 'CKRS0100 - O PROCESSO NAO SE ENCONTRA CADA
      -                      'STRADO'  TO ERR-TEXTO

                   WHEN OTHER
                        MOVE 'CKRS0100 - ERRO NAO IDENTIFICADO'
                                       TO ERR-TEXTO
                        DISPLAY '************** GFCT2365 **************'
                        DISPLAY '*                                    *'
                        DISPLAY '* CK01-CODIGO-RETORNO : '
                                   CK01-CODIGO-RETORNO    '           *'
                        DISPLAY '* CK01-CODIGO-MENSAGEM: '
                                   CK01-CODIGO-MENSAGEM         '     *'
                        DISPLAY '*                                    *'
                        DISPLAY '************** GFCT2365 **************'
               END-EVALUATE
           ELSE
               MOVE 'CKRS0100 - ERRO GRAVE - RETURN <> DE ZEROS E 8'
                                       TO ERR-TEXTO
               DISPLAY '************** GFCT2365 **************'
               DISPLAY '*                                    *'
               DISPLAY '* CK01-CODIGO-RETORNO : '
                          CK01-CODIGO-RETORNO    '           *'
               DISPLAY '* CK01-CODIGO-MENSAGEM: '
                          CK01-CODIGO-MENSAGEM         '     *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT2365 **************'
           END-IF.

      *----------------------------------------------------------------*
       9310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA TRATAR ERRO                                         *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2365'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           ELSE
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
