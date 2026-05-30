      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT5323.
       AUTHOR.     INFOSERVER.
      *================================================================*
      *                    C P M  -  S I S T E M A S.                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5323                                    *
      *    PROGRAMADOR.:   INFOSERVER                                  *
      *    ANALISTA....:   INFOSERVER                                  *
      *    DATA........:   MARCO/2017                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA DE TARIFAS EXCLUSIVAS              *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.TTARIF_EXCVD_GSTOR     GFCTB0P1       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    GFCTWAS7 - ENTRADA                                          *
      *    GFCTWAS8 - SAIDA                                            *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA TO VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(011)         VALUE SPACES.

       01  WRK-CTWAS8-QTDE-TOT-REG     PIC 9(06)  COMP-3   VALUE ZEROS.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

       01  WRK-NUM-5-S                  PIC -9(005)        VALUE ZEROS.
       01  WRK-NUM-5-SR   REDEFINES WRK-NUM-5-S.
           05 FILLER                   PIC  X(001).
           05 WRK-NUM-5-S-S            PIC  9(005).

       01  WRK-NUM-11-S                 PIC -9(011)        VALUE ZEROS.
       01  WRK-NUM-11-SR   REDEFINES WRK-NUM-11-S.
           05 FILLER                   PIC  X(001).
           05 WRK-NUM-11-S-S           PIC  9(011).

       01  WRK-NUM-9-S                  PIC -9(009)        VALUE ZEROS.
       01  WRK-NUM-9-SR   REDEFINES WRK-NUM-9-S.
           05 FILLER                   PIC  X(001).
           05 WRK-NUM-9-S-S            PIC  9(009).
           05 WRK-NUM-9-S-ST REDEFINES WRK-NUM-9-S-S.
              07 FILLER                   PIC  9(002).
              07 WRK-NUMN-7               PIC  X(007).

       01  WRK-GFCTB0P1-NULL.
           03 WRK-DFIM-VGCIA-TARIF-NULL     PIC S9(004) COMP   VALUE 0.
           03 WRK-CDEPDC-AUTRZ-ACSSO-NULL   PIC S9(004) COMP   VALUE 0.
           03 WRK-CFUNC-MANUT-INCL-NULL     PIC S9(004) COMP   VALUE 0.
           03 WRK-HINCL-REG-NULL            PIC S9(004) COMP   VALUE 0.
           03 WRK-CFUNC-MANUT-EXCL-NULL     PIC S9(004) COMP   VALUE 0.
           03 WRK-HMANUT-REG-NULL           PIC S9(004) COMP   VALUE 0.
           03 WRK-CINDCD-EXCL-LOGCA-NULL    PIC S9(004) COMP   VALUE 0.
           03 WRK-RJUST-INCL-AUTRZ-NULL     PIC S9(004) COMP   VALUE 0.
           03 WRK-RJUST-EXCL-AUTRZ-NULL     PIC S9(004) COMP   VALUE 0.

       01  WRK-SINAL                   PIC -9(03)          VALUE ZEROS.
       01  FILLER    REDEFINES WRK-SINAL.
           03  FILLER                  PIC X(01).
           03  WRK-SINAL-S            PIC 9(03).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM SENH0315 *'.
      *---------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           05  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
           05  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           05  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*     05  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
ST25X6*     05  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
ST25X6*     05  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
           05  WRK-CNPJ-PRI-0315       PIC  X(009)         VALUE SPACES.
           05  WRK-CNPJ-FIL-0315       PIC  X(004)         VALUE SPACES.
           05  WRK-CNPJ-CON-0315       PIC  X(002)         VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCT1V.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCT1X.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA TO TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0P1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWAS7'.

       COPY 'GFCTWAS8'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING CTWAS7-ENTRADA
                                                      CTWAS8-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *INICIA O PROGRAMA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2950-CONSULTAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA, VERIFICA DISPONIBILIDADE DO SISTEMA E CON- *
      *SISTE DADOS.                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          CTWAS8-SAIDA.

           INITIALIZE CTWAS8-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE CTWAS7-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE CTWAS7-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE CTWAS7-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO CTWAS8-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5323'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 9                      TO CTWAS8-ERRO
               MOVE 'GFCT5323'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              CTWAS8-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO CTWAS8-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO CTWAS8-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                           TO GFCT0M-TEXTO
                                              CTWAS8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO CTWAS8-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5323'             TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 1007                       TO CTWAS8-LL.
           MOVE CTWAS7-ZZ                  TO CTWAS8-ZZ.

           MOVE CTWAS7-TRANSACAO           TO CTWAS8-TRANSACAO.
           MOVE CTWAS7-FUNCAO              TO CTWAS8-FUNCAO.
           MOVE CTWAS7-FUNC-BDSCO          TO CTWAS8-FUNC-BDSCO.

           MOVE 'S'                        TO CTWAS8-FIM.

           MOVE ZEROS                      TO CTWAS8-ERRO
                                              CTWAS8-COD-SQL-ERRO
                                              CTWAS8-COD-MSG-ERRO
                                              GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG            TO CTWAS8-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (CTWAS7-TRANSACAO         EQUAL SPACES OR LOW-VALUES) OR
              (CTWAS7-FUNCAO            EQUAL SPACES OR LOW-VALUES) OR
              (CTWAS7-FUNC-BDSCO        EQUAL SPACES OR LOW-VALUES) OR
              (CTWAS7-PONT-CSERVC-TARIF EQUAL ZEROS)                OR
              (CTWAS7-PONT-DATA-INICIO  EQUAL SPACES OR LOW-VALUES) OR
              (CTWAS7-PONT-SEQ-MOVTO    EQUAL ZEROS)
               MOVE  1                  TO CTWAS8-ERRO
               MOVE  ZEROS              TO CTWAS8-COD-SQL-ERRO
               MOVE  0001               TO CTWAS8-COD-MSG-ERRO
               MOVE '0001'              TO GFCTG2-COD-MSG
               MOVE 'S'                 TO CTWAS8-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO CTWAS8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE CTWAS7-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE CTWAS7-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE CTWAS7-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO CTWAS8-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5323'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO CTWAS8-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5323'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 1                      TO CTWAS8-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO CTWAS8-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO CTWAS8-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO CTWAS8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA ERROS DB2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1321-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5323'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  9                     TO CTWAS8-ERRO.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          WRK-SINAL
           MOVE WRK-SINAL-S            TO CTWAS8-COD-SQL-ERRO.
           MOVE SPACES                 TO CTWAS8-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG WRK-NOM-TAB
           DELIMITED BY '  '           INTO CTWAS8-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2560-MOVER-CAMPOS               SECTION.
      *----------------------------------------------------------------*

           MOVE 1007                   TO CTWAS8-LL

           INSPECT DINIC-VGCIA-TARIF  OF GFCTB0P1  REPLACING ALL
                                                     '.'    BY '/'.
           MOVE CSERVC-TARIF       OF GFCTB0P1
                                      TO WRK-NUM-5-S.
           MOVE WRK-NUM-5-S-S         TO CTWAS8-CSERVC-TARIF
           PERFORM 2570-OBTEM-SERV-DESC.
           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8  TO CTWAS8-DESC-TARIFA
           MOVE DINIC-VGCIA-TARIF  OF GFCTB0P1
                                      TO CTWAS8-DATA-INICIO.
           MOVE CSEQ-MOVTO         OF GFCTB0P1
                                      TO WRK-NUM-11-S.
           MOVE WRK-NUM-11-S-S        TO CTWAS8-SEQ-MOVTO
           INSPECT DFIM-VGCIA-TARIF   OF GFCTB0P1  REPLACING ALL
                                                     '.'    BY '/'.
           MOVE DFIM-VGCIA-TARIF   OF GFCTB0P1
                                      TO CTWAS8-DATA-FIM
           MOVE CDEPDC-AUTRZ-ACSSO OF GFCTB0P1
                                      TO WRK-NUM-5-S.
           MOVE WRK-NUM-5-S-S         TO CTWAS8-DEPDC-AUTRZ
           PERFORM 2580-OBTER-NOME-DEPDC.
           MOVE GFCT1X-DESC-DEPDC(1)  TO CTWAS8-DESC-DEPDC
           MOVE CFUNC-MANUT-INCL   OF GFCTB0P1
                                       TO WRK-NUM-9-S.
           MOVE WRK-NUM-9-S-S          TO CTWAS8-FUNC-INCL
           MOVE WRK-NUMN-7             TO WRK-COD-FUNCIONAL-0315.
           PERFORM 2590-OBTEM-FUNC-NOME
           MOVE WRK-NOME-FUNC-0315    TO CTWAS8-FUNC-NOME-INCL
           MOVE HINCL-REG OF GFCTB0P1 TO CTWAS8-DATA-HORA-INCL
           MOVE RJUSTF-INCL-AUTRZ-TEXT OF GFCTB0P1 TO CTWAS8-JUST-INCL
           IF WRK-CFUNC-MANUT-EXCL-NULL GREATER -1
               MOVE CFUNC-MANUT-EXCL   OF GFCTB0P1
                                          TO WRK-NUM-9-S
               MOVE WRK-NUM-9-S-S         TO CTWAS8-FUNC-EXCL
               MOVE WRK-NUMN-7            TO WRK-COD-FUNCIONAL-0315
               PERFORM 2590-OBTEM-FUNC-NOME
               MOVE WRK-NOME-FUNC-0315    TO CTWAS8-FUNC-NOME-EXCL
               MOVE HMANUT-REG OF GFCTB0P1 TO CTWAS8-DATA-HORA-EXCL
               MOVE RJUSTF-EXCL-AUTRZ-TEXT OF GFCTB0P1
                                          TO CTWAS8-JUST-EXCL
           END-IF
           MOVE 'S'                    TO CTWAS8-FIM.
           MOVE CTWAS8-QTDE-TOT-REG     TO WRK-CTWAS8-QTDE-TOT-REG.
           ADD 1                        TO WRK-CTWAS8-QTDE-TOT-REG.
           MOVE WRK-CTWAS8-QTDE-TOT-REG TO CTWAS8-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       2560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2570-OBTEM-SERV-DESC            SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0P1
                                       TO CSERVC-TARIF OF GFCTB0D8.
           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D8'      TO WRK-NOM-TAB
               MOVE 'SERVC_TARIF_PRIN' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0025'             TO GFCT0M-LOCAL
               MOVE  0012              TO CTWAS8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO CTWAS8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.


      *----------------------------------------------------------------*
       2570-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR GFCT5006 - OBTER NOME AGENCIA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2580-OBTER-NOME-DEPDC           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS
           INITIALIZE                     GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS

           MOVE 'GFCT5006'             TO WRK-MODULO

           MOVE CTWAS7-TRANSACAO       TO GFCT1V-TRANSACAO
           MOVE CTWAS7-FUNCAO          TO GFCT1V-FUNCAO
           MOVE CTWAS7-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO
           MOVE 1                      TO GFCT1V-QTDE-OCOR
           MOVE 'N'                    TO GFCT1V-FIM
           MOVE CTWAS8-DEPDC-AUTRZ     TO GFCT1V-COD-DEPDC(1)

           CALL  WRK-MODULO         USING GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE      NOT EQUAL ZEROS
               MOVE 1                  TO CTWAS8-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5323'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO          EQUAL 9
               MOVE 'S'                TO CTWAS8-FIM
               MOVE 1                  TO CTWAS8-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO CTWAS8-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO CTWAS8-COD-MSG-ERRO
               MOVE GFCT1X-DESC-MSG-ERRO
                                       TO CTWAS8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.


      *----------------------------------------------------------------*
       2580-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA O MODULO SENH0315                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2590-OBTEM-FUNC-NOME            SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.

           CALL 'SENH0315'             USING WRK-VERSAO-0315
                                             WRK-MENSAGEM-0315
                                             WRK-COD-FUNCIONAL-0315
                                             WRK-NOME-FUNC-0315
                                             WRK-COD-BANCO-0315
                                             WRK-NOME-BANCO-0315
                                             WRK-CNPJ-BANCO-0315
                                             WRK-COD-JUNCAO-0315
                                             WRK-NOME-DEPTO-0315
                                             WRK-SECAO-0315
                                             WRK-STATUS-0315.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 04
               MOVE 'S'                TO CTWAS8-FIM
               MOVE 1                  TO CTWAS8-ERRO
               MOVE RETURN-CODE        TO CTWAS8-COD-SQL-ERRO
               MOVE 0056               TO CTWAS8-COD-MSG-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO CTWAS8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MENSAGEM-0315   EQUAL 001
                   MOVE 'S'                TO CTWAS8-FIM
                   MOVE 1                  TO CTWAS8-ERRO
                   MOVE RETURN-CODE        TO CTWAS8-COD-SQL-ERRO
                   MOVE 0057               TO CTWAS8-COD-MSG-ERRO
                   MOVE '0057'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO CTWAS8-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 154
                   MOVE 'S'                TO CTWAS8-FIM
                   MOVE 1                  TO CTWAS8-ERRO
                   MOVE RETURN-CODE        TO CTWAS8-COD-SQL-ERRO
                   MOVE 0058               TO CTWAS8-COD-MSG-ERRO
                   MOVE '0058'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO CTWAS8-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 242
                   MOVE 'S'                TO CTWAS8-FIM
                   MOVE 1                  TO CTWAS8-ERRO
                   MOVE RETURN-CODE        TO CTWAS8-COD-SQL-ERRO
                   MOVE 0059               TO CTWAS8-COD-MSG-ERRO
                   MOVE '0059'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO CTWAS8-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 009
                   MOVE 'S'                TO CTWAS8-FIM
                   MOVE 1                  TO CTWAS8-ERRO
                                              CTWAS8-COD-MSG-ERRO
                   MOVE RETURN-CODE        TO CTWAS8-COD-SQL-ERRO
                   MOVE '0001'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO CTWAS8-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 138
                   MOVE 'S'                TO CTWAS8-FIM
                   MOVE 1                  TO CTWAS8-ERRO
                   MOVE RETURN-CODE        TO CTWAS8-COD-SQL-ERRO
                   MOVE 0060               TO CTWAS8-COD-MSG-ERRO
                   MOVE '0060'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO CTWAS8-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2590-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTAR REGISTRO                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2950-CONSULTAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE CTWAS7-PONT-CSERVC-TARIF  TO CSERVC-TARIF   OF GFCTB0P1
           INSPECT CTWAS7-PONT-DATA-INICIO REPLACING ALL '/'  BY '.'.
           MOVE CTWAS7-PONT-DATA-INICIO
                                       TO DINIC-VGCIA-TARIF OF GFCTB0P1
           MOVE CTWAS7-PONT-SEQ-MOVTO  TO CSEQ-MOVTO        OF GFCTB0P1

           EXEC SQL
             SELECT  CSERVC_TARIF,
                     DINIC_VGCIA_TARIF,
                     CSEQ_MOVTO,
                     DFIM_VGCIA_TARIF,
                     CDEPDC_AUTRZ_ACSSO,
                     CFUNC_MANUT_INCL,
                     HINCL_REG,
                     CFUNC_MANUT_EXCL,
                     HMANUT_REG,
                     CINDCD_EXCL_LOGCA,
                     RJUSTF_INCL_AUTRZ,
                     RJUSTF_EXCL_AUTRZ
             INTO
                     :GFCTB0P1.CSERVC-TARIF,
                     :GFCTB0P1.DINIC-VGCIA-TARIF,
                     :GFCTB0P1.CSEQ-MOVTO,
                     :GFCTB0P1.DFIM-VGCIA-TARIF
                         :WRK-DFIM-VGCIA-TARIF-NULL,
                     :GFCTB0P1.CDEPDC-AUTRZ-ACSSO
                         :WRK-CDEPDC-AUTRZ-ACSSO-NULL,
                     :GFCTB0P1.CFUNC-MANUT-INCL
                         :WRK-CFUNC-MANUT-INCL-NULL,
                     :GFCTB0P1.HINCL-REG
                         :WRK-HINCL-REG-NULL,
                     :GFCTB0P1.CFUNC-MANUT-EXCL
                         :WRK-CFUNC-MANUT-EXCL-NULL,
                     :GFCTB0P1.HMANUT-REG
                         :WRK-HMANUT-REG-NULL,
                     :GFCTB0P1.CINDCD-EXCL-LOGCA
                         :WRK-CINDCD-EXCL-LOGCA-NULL,
                     :GFCTB0P1.RJUSTF-INCL-AUTRZ
                         :WRK-RJUST-INCL-AUTRZ-NULL,
                     :GFCTB0P1.RJUSTF-EXCL-AUTRZ
                         :WRK-RJUST-EXCL-AUTRZ-NULL
             FROM   DB2PRD.TTARIF_EXCVD_GSTOR
             WHERE    CSERVC_TARIF       = :GFCTB0P1.CSERVC-TARIF
              AND     DINIC_VGCIA_TARIF  = :GFCTB0P1.DINIC-VGCIA-TARIF
              AND     CSEQ_MOVTO         = :GFCTB0P1.CSEQ-MOVTO
           END-EXEC.

           IF (SQLCODE                 EQUAL ZEROS)
              IF WRK-DFIM-VGCIA-TARIF-NULL LESS ZEROS
                 MOVE ZEROS TO DFIM-VGCIA-TARIF   OF GFCTB0P1
              END-IF
              IF WRK-CDEPDC-AUTRZ-ACSSO-NULL LESS ZEROS
                 MOVE ZEROS TO CDEPDC-AUTRZ-ACSSO OF GFCTB0P1
              END-IF
              IF WRK-CFUNC-MANUT-INCL-NULL LESS ZEROS
                 MOVE ZEROS TO CFUNC-MANUT-INCL   OF GFCTB0P1
              END-IF
              IF WRK-HINCL-REG-NULL LESS ZEROS
                 MOVE ZEROS TO HINCL-REG          OF GFCTB0P1
              END-IF
              IF WRK-CFUNC-MANUT-EXCL-NULL LESS ZEROS
                 MOVE ZEROS TO CFUNC-MANUT-EXCL   OF GFCTB0P1
              END-IF
              IF WRK-HMANUT-REG-NULL LESS ZEROS
                 MOVE ZEROS TO HMANUT-REG         OF GFCTB0P1
              END-IF
              IF WRK-CINDCD-EXCL-LOGCA-NULL LESS ZEROS
                 MOVE ZEROS TO CINDCD-EXCL-LOGCA  OF GFCTB0P1
              END-IF
              IF WRK-RJUST-INCL-AUTRZ-NULL LESS ZEROS
                 MOVE ZEROS TO RJUSTF-INCL-AUTRZ  OF GFCTB0P1
              END-IF
              IF WRK-RJUST-EXCL-AUTRZ-NULL LESS ZEROS
                 MOVE ZEROS TO RJUSTF-EXCL-AUTRZ  OF GFCTB0P1
              END-IF
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0P1'      TO WRK-NOM-TAB
               MOVE 'TTARIF_EXCVD_GST' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0012              TO CTWAS8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO CTWAS8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 1                  TO CTWAS8-ERRO
               MOVE ZEROS              TO CTWAS8-COD-SQL-ERRO
               MOVE 0010               TO CTWAS8-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO CTWAS8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.
           PERFORM 2560-MOVER-CAMPOS.

      *----------------------------------------------------------------*
       2950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *RETORNA AO CHAMADOR                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
