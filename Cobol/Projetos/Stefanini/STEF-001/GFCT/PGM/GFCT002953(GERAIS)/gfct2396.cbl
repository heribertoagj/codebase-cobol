      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2396.
       AUTHOR.     JANIEL FREITAS.
      *================================================================*
      *                    S O N D A  P R O C W O R K                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2396                                    *
      *    PROGRAMADOR.:   JANIEL FREITAS - SONDA PROCWORK / GP.50     *
      *    ANALISTA....:   JANIEL FREITAS - SONDA PROCWORK / GP.50     *
      *    DATA........:   15/09/2009                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO    :   EXPURGO DE REGISTROS INFERIORES A 12 MESES -*
      *                    DPROCM_ATUAL DO ARQUIVO IMAGEM MENSAL TARIF.*
      *                    PESSOA FISICA OU JURIDICA.                  *
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *      DDNAME                                  INCLUDE/BOOK      *
      *      TARIFMES                                  GFCTWAI7        *
      *      ARQRELAT                                  GFCTWAI7        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *         DB2                                                    *
      *           TABLE                              INCLUDE/BOOK      *
      *           DB2PRD.PARM_DATA_PROCM               GFCTB0A1        *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *      I#BRAD7C - AREA DE COMUNICACAO COM A POOL7100.            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *      BRAD1050 - CONEXAO COM O DB2.                             *
      *      BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO.  *
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

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT TARIFMES ASSIGN      TO UT-S-TARIFMES
                      FILE STATUS      IS WRK-FS-TARIFMES.

           SELECT ARQRELAT ASSIGN      TO UT-S-ARQRELAT
                      FILE STATUS      IS WRK-FS-ARQRELAT.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA                                  *
      *            ORG. SEQUENTIAL     -  LRECL = 110                  *
      *----------------------------------------------------------------*

       FD  TARIFMES
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAI7'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA                                    *
      *            ORG. SEQUENTIAL     -  LRECL = 110                  *
      *----------------------------------------------------------------*

       FD  ARQRELAT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAI7'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '***  GFCT2396 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-TARIFMES       PIC  9(010) COMP-3  VALUE ZEROS.
           05 ACU-GRAV-ARQRELAT        PIC  9(010) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC  X(008)         VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.
           05  WRK-ANO-DPROCM-COMP     PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-DATA-R                  PIC  X(010)   VALUE '00.00.0000'.
       01  FILLER                      REDEFINES     WRK-DATA-R.
           05  WRK-DATA-DIA            PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-DATA-MES            PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-DATA-ANO            PIC  9(004).

       01  WRK-DPROCM-ATUAL-R.
           05  WRK-DPROCM-ANO          PIC  9(004)         VALUE ZEROS.
           05  WRK-DPROCM-MES          PIC  9(002)         VALUE ZEROS.

       01  WRK-DOCOR-EVNTO-R.
           05  WRK-DOCOR-ANO           PIC  9(004)         VALUE ZEROS.
           05  WRK-DOCOR-MES           PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-TARIFMES             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQRELAT             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGEM DE ERRO ***'.
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
           05  FILLER                  PIC  X(007)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT2396 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0500-OBTER-DPROCM-ATUAL.

           PERFORM 0600-VERIFICAR-VAZIO.

           PERFORM 0800-PROCESSAR UNTIL WRK-FS-TARIFMES EQUAL '10'.

           PERFORM 1200-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT TARIFMES
               OUTPUT ARQRELAT.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE STATUS DOS ARQUIVOS DE ENTRADA E SAIDA          *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-TARIFMES.

           PERFORM 0400-TESTAR-FS-ARQRELAT.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO TARIFMES                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-TARIFMES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-TARIFMES         NOT EQUAL '00'
               MOVE 'TARIFMES'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-TARIFMES    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ARQRELAT                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ARQRELAT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQRELAT         NOT EQUAL '00'
               MOVE 'ARQRELAT'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ARQRELAT    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DPROCM_ATUAL (ANO E MES)                              *
      *----------------------------------------------------------------*
       0500-OBTER-DPROCM-ATUAL         SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                    DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                    CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0001'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           ELSE
               MOVE DPROCM-ATUAL       OF GFCTB0A1 TO WRK-DATA-R
               MOVE WRK-DATA-ANO       TO WRK-ANO-DPROCM-COMP
               COMPUTE WRK-ANO-DPROCM-COMP = WRK-ANO-DPROCM-COMP - 1
               MOVE WRK-ANO-DPROCM-COMP
                                       TO WRK-DPROCM-ANO
               MOVE WRK-DATA-MES       TO WRK-DPROCM-MES
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR SE ARQUIVO DE ENTRADA ESTA VAZIO      *
      *----------------------------------------------------------------*
       0600-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0700-LER-TARIFMES.

           IF  WRK-FS-TARIFMES         EQUAL '10'
               DISPLAY '*********** GFCT2396 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO TARIFMES VAZIO    *'
               DISPLAY '*   PROCESSAMENTO  ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2396 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO TARIFMES                                 *
      *----------------------------------------------------------------*
       0700-LER-TARIFMES               SECTION.
      *----------------------------------------------------------------*

           READ TARIFMES.

           IF  WRK-FS-TARIFMES         EQUAL '10'
               GO                      TO 0700-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-TARIFMES.

           MOVE WAI7-DOCOR-EVNTO       OF TARIFMES
                                       TO WRK-DATA-R.

           MOVE WRK-DATA-ANO           TO WRK-DOCOR-ANO.
           MOVE WRK-DATA-MES           TO WRK-DOCOR-MES.

           ADD 1                       TO ACU-LIDOS-TARIFMES.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       0800-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-DOCOR-EVNTO-R       NOT LESS WRK-DPROCM-ATUAL-R
               PERFORM 1100-GRAVAR-ARQRELAT
           END-IF.

           PERFORM 0700-LER-TARIFMES.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO DE SAIDA                                     *
      *----------------------------------------------------------------*
       1100-GRAVAR-ARQRELAT            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WAI7-REL-SUST
                                       OF ARQRELAT.

           MOVE WAI7-REL-SUST          OF TARIFMES
                                       TO WAI7-REL-SUST
                                       OF ARQRELAT.

           WRITE WAI7-REL-SUST         OF ARQRELAT.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-ARQRELAT.

           ADD 1                       TO ACU-GRAV-ARQRELAT.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1200-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           CLOSE TARIFMES
                 ARQRELAT.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           PERFORM 1300-EMITIR-DISPLAY.

           STOP RUN.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DOS DISPLAYS FINAIS DO PROGRAMA         *
      *----------------------------------------------------------------*
       1300-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT2396 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-TARIFMES     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM TARIFMES......: '
                                                WRK-MASCARA '        *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAV-ARQRELAT      TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO ARQRELAT...: '
                                                WRK-MASCARA '        *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2396 ********************'.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2396'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
