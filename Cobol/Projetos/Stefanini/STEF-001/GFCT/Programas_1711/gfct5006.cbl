      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT5006.
       AUTHOR.     BRUNO VARASCHIN PALAORO.
      *===============================================================*
      *                    C P M  -  S I S T E M A S                  *
      *---------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5006                                   *
      *    PROGRAMADOR.:   BRUNO VARASCHIN PALAORO - CPM PATO BRANCO  *
      *    ANALISTA CPM:   MARCIO C. CUSTIN        - CPM PATO BRANCO  *
      *    ANALISTA....:   RICARDO PEREIRA         - PROCWORK GP.67   *
      *    DATA........:   01/12/2004                                 *
      *---------------------------------------------------------------*
      *    OBJETIVO....:   OBTER DESCRICAO DA DEPENDENCIA.            *
      *---------------------------------------------------------------*
      *    BCO DE DADOS:                                              *
      *                DB2                                            *
      *                TABLE                           INCLUDE/BOOK   *
      *                    DB2PRD.DEPDC_BDSCO_ONLINE     MESUB010     *
      *---------------------------------------------------------------*
      *    INC'S.......:                                              *
      *    I#GFCT0M - COMUNICACAO WEB X IMS - ERROS                   *
      *    I#GFCT1V - COMUNICACAO WEB X IMS - ENTRADA                 *
      *    I#GFCT1X - COMUNICACAO WEB X IMS - SAIDA                   *
      *---------------------------------------------------------------*
      *    MODULOS.....:                                              *
      *    GFCT5000 - OBTER DESCRICAO DE MENSAGENS                    *
      *    POOL0081 - ALOCACAO DINAMICA DE MODULOS                    *
      *===============================================================*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * ***  HEXASOLUTION - 03/2009 - CONVERSAO FAIXA DE AGENCIAS  *** *
HEXA  *----------------------------------------------------------------*
HEXA  *
           EJECT
      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT5006 - INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADORES ***'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(002)         VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO GFCT5000 ***'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-ENVIA.
               05  WRK-5000-AMBIENTE   PIC  X(001)         VALUE SPACES.
               05  WRK-5000-COD-MSG    PIC  9(004)         VALUE ZEROS.
           03  WRK-5000-RECEBE.
               05  WRK-5000-COD-RET    PIC  9(002)         VALUE ZEROS.
               05  WRK-5000-COD-SQL-ER PIC  9(003)         VALUE ZEROS.
               05  WRK-5000-DESC-MSG   PIC  X(070)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** INDICADORES DE NULIDADE ***'.
      *---------------------------------------------------------------*

       01  WRK-MESUB010-NULL.
           03 WRK-IDEPDC-NULL          PIC S9(04) COMP VALUE +0.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE MESUB010
           END-EXEC.
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY I#HEXA01.
HEXA  *

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT5006 - FIM DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCT1V.
       COPY I#GFCT1X.
       COPY I#GFCT0M.

           EJECT
      *===============================================================*
       PROCEDURE DIVISION              USING GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.
      *===============================================================*

      *---------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *---------------------------------------------------------------*
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
HEXA  *

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-PROCESSAR.

           PERFORM 300000-FINALIZAR.

      *---------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *---------------------------------------------------------------*

           INITIALIZE GFCT1X-SAIDA.

           MOVE GFCT1V-LL              TO GFCT1X-LL.
           MOVE GFCT1V-ZZ              TO GFCT1X-ZZ.
           MOVE GFCT1V-TRANSACAO       TO GFCT1X-TRANSACAO.
           MOVE GFCT1V-FUNCAO          TO GFCT1X-FUNCAO.
           MOVE GFCT1V-FUNC-BDSCO      TO GFCT1X-FUNC-BDSCO.

           IF  GFCT1V-QTDE-OCOR        IS NUMERIC
               MOVE GFCT1V-QTDE-OCOR   TO GFCT1X-QTDE-OCOR
           END-IF.

           MOVE 'N'                    TO GFCT1X-FIM.
           MOVE ZEROS                  TO WRK-5000-COD-MSG.

           PERFORM 110000-ACESSAR-GFCT5000.

           MOVE WRK-5000-DESC-MSG      TO GFCT1X-DESC-MSG-ERRO.

           PERFORM 120000-CONSISTIR-DADOS.

      *---------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       110000-ACESSAR-GFCT5000         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5000-RECEBE.

           MOVE 'O'                        TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'                 TO WRK-MODULO.

           CALL 'POOL0081'                 USING WRK-MODULO
                                                 WRK-5000-ENVIA
                                                 WRK-5000-RECEBE.

           IF  WRK-5000-COD-RET            NOT EQUAL ZEROS
               IF  WRK-5000-COD-RET        EQUAL 01
                   MOVE 1                  TO GFCT1X-ERRO
                   MOVE ZEROS              TO GFCT1X-COD-SQL-ERRO
                                              GFCT1X-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG  TO GFCT1X-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5000-COD-RET    EQUAL 99
                       MOVE 1              TO GFCT1X-ERRO
                       MOVE WRK-5000-COD-SQL-ER
                                           TO GFCT1X-COD-SQL-ERRO
                       MOVE ZEROS          TO GFCT1X-COD-MSG-ERRO
                       MOVE WRK-5000-DESC-MSG
                                           TO GFCT1X-DESC-MSG-ERRO
                   END-IF
               END-IF
               INITIALIZE GFCT1X-DADOS
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       120000-CONSISTIR-DADOS          SECTION.
      *---------------------------------------------------------------*

           IF  GFCT1V-TRANSACAO        EQUAL SPACES OR
               GFCT1V-FUNCAO           EQUAL SPACES OR
               GFCT1V-FUNC-BDSCO       EQUAL SPACES OR
               GFCT1V-QTDE-OCOR        NOT NUMERIC  OR
               GFCT1V-FIM              NOT EQUAL 'S' AND 'N'
               MOVE ZEROS              TO GFCT1X-COD-SQL-ERRO
               MOVE 1                  TO GFCT1X-ERRO
                                          GFCT1X-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 110000-ACESSAR-GFCT5000
               MOVE WRK-5000-DESC-MSG  TO GFCT1X-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-MONTAR-SAIDA
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER GFCT1V-QTDE-OCOR
                OR IND-1               GREATER 5.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-MONTAR-SAIDA             SECTION.
      *----------------------------------------------------------------*

           PERFORM 211000-ACESSAR-MESUB010.

           COMPUTE GFCT1X-LL           =  (GFCT1X-LL + 145).

           IF  SQLCODE                 EQUAL +100
               PERFORM 212000-TRATAR-SQLCODE-100
           ELSE
               PERFORM 213000-TRATAR-SQLCODE-NORMAL
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211000-ACESSAR-MESUB010         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT1V-COD-DEPDC(IND-1)    TO CDEPDC       OF MESUB010.
           MOVE 237                        TO CEMPR-INC    OF MESUB010.

           EXEC SQL
             SELECT
                   IDEPDC
             INTO
                   :MESUB010.IDEPDC
                     :WRK-IDEPDC-NULL
             FROM   DB2PRD.DEPDC_BDSCO_ONLINE
             WHERE
                   CEMPR_INC          = :MESUB010.CEMPR-INC          AND
                   CDEPDC             = :MESUB010.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT5006'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'DEPDC_BDSCO_ONLINE'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCT1X-COD-SQL-ERRO
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT1X-ERRO
               MOVE  12                TO GFCT1X-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 110000-ACESSAR-GFCT5000
               STRING
                   WRK-5000-DESC-MSG,      ' - ',
                   'MESUB010'
               DELIMITED BY '  '       INTO GFCT1X-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212000-TRATAR-SQLCODE-100       SECTION.
      *----------------------------------------------------------------*

           MOVE 01                     TO GFCT1X-ERRO.
           MOVE SQLCODE                TO GFCT1X-COD-SQL-ERRO-GEN(IND-1)
                                          GFCT1X-COD-SQL-ERRO.

HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  * HX_IF GFCT1V-COD-DEPDC(IND-1)  > 3999
HEXA  *
HEXA       MOVE        GFCT1V-COD-DEPDC(IND-1) TO WRK-MESUX0-AGENCIA
HEXA       PERFORM     9990-10-CHAMA-ROTINA-FX-AG
HEXA  *
HEXA       IF WRK-NAO-EH-FX-AG
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  *
              MOVE 607                 TO GFCT1X-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
           ELSE
              MOVE 40                     TO GFCT1X-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
           END-IF.

           PERFORM 110000-ACESSAR-GFCT5000.

           MOVE WRK-5000-DESC-MSG      TO GFCT1X-DESC-MSG-ERRO.
           MOVE 1                      TO GFCT1X-ERRO-DEPDC(IND-1).
           MOVE GFCT1V-COD-DEPDC(IND-1)
                                       TO GFCT1X-COD-DEPDC(IND-1).
           MOVE SPACES                 TO GFCT1X-DESC-DEPDC(IND-1).
           MOVE 25                     TO GFCT1X-COD-MSG-ERRO-GEN(IND-1)
                                          WRK-5000-COD-MSG.

           PERFORM 110000-ACESSAR-GFCT5000.

           MOVE WRK-5000-DESC-MSG    TO GFCT1X-DESC-MSG-ERRO-GEN(IND-1).

      *----------------------------------------------------------------*
       212000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213000-TRATAR-SQLCODE-NORMAL    SECTION.
      *----------------------------------------------------------------*

           IF  WRK-IDEPDC-NULL         NOT EQUAL ZEROS
               MOVE SPACES             TO IDEPDC   OF MESUB010
           END-IF.

           MOVE CDEPDC OF MESUB010     TO GFCT1X-COD-DEPDC(IND-1).
           MOVE IDEPDC OF MESUB010     TO GFCT1X-DESC-DEPDC(IND-1).

      *----------------------------------------------------------------*
       213000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO GFCT1X-FIM.

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE CONVERSAO DE AGENCIA
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-10-CHAMA-ROTINA-FX-AG SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       CALL        WRK-MODULO-MESUX0 USING WRK-AREA-MESUX0.
HEXA  *
HEXA       MOVE        ZEROS       TO          WRK-MESUX0-AGENCIA.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
