      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5046.
       AUTHOR. FERNANDO NAIM SCHMITZ.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5046                                     *
      *    PROGRAMADOR  : FERNANDO NAIM SCHMITZ         - CPM/FPOLIS   *
      *    ANALISTA CPM : MICHELLI VENTURA BORGES       - CPM/FPOLIS   *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK    - GRUPO 50     *
      *    DATA         : 24/05/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA COMBO DE TARIFAS - TABELA GFCTB032.      *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0D8          *
      *       DB2PRD.CTRL_ESTRN_PCIAL                GFCTB032          *
      *       DB2PRD.PARM_DATA_PROCM                 GFCTB0A1          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5507 - VERIFICA MASTER                                *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      POOL1470 - CONSISTE DATAS COM SECULO                      *
      *      SENH0315 - OBTER JUNCAO                                   *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7102                                     *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
        CONFIGURATION                  SECTION.
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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT5046  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC 9(03)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-FINALIZAR               PIC X(01)           VALUE SPACES.
       01  WRK-ERRO-DATA               PIC X(01)           VALUE SPACES.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.
       01  WRK-DT-DINIC                PIC 9(08)           VALUE ZEROS.
       01  WRK-DT-REFER                PIC 9(08)           VALUE ZEROS.
       01  WRK-DATA-PONTEIRO           PIC X(10)           VALUE SPACES.
       01  WRK-AUX-TARIFA              PIC 9(05)           VALUE ZEROS.
       01  WRK-TIPO-VIGENCIA           PIC X(01)           VALUE SPACES.

       01  WRK-DATA-DD-MM-AAAA.
           03  WRK-DATA-DD             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-DATA-MM             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-DATA-AAAA           PIC 9(04)           VALUE ZEROS.

       01  WRK-DATA-REFER.
           03  WRK-DTREF-DIA           PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-DTREF-MES           PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-DTREF-ANO           PIC 9(04)           VALUE ZEROS.

       01  WRK-CAD-DATA.
           03  WRK-CAD-DIA             PIC 9(02)           VALUE ZEROS.
           03  WRK-CAD-MES             PIC 9(02)           VALUE ZEROS.
           03  WRK-CAD-ANO             PIC 9(04)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5000   *'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE   PIC X(01)           VALUE SPACES.
               05  WRK-5000-COD-MSG    PIC 9(04)           VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO
                                       PIC 9(02)           VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO
                                       PIC 9(03)           VALUE ZEROS.
               05  WRK-5000-DESC-MSG   PIC X(70)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5507   *'.
      *----------------------------------------------------------------*

       01  WRK-5507-AREA.
           03  WRK-5507-AREA-ENVIA.
               05  WRK-5507-AMBIENTE   PIC X(01)           VALUE SPACES.
               05  WRK-5507-CJUNC-DEPDC
                                       PIC 9(05)           VALUE ZEROS.
           03  WRK-5507-AREA-RECEBE.
               05  WRK-5507-COD-RETORNO
                                       PIC 9(02)           VALUE ZEROS.
               05  WRK-5507-COD-SQL-ERRO
                                       PIC 9(03)           VALUE ZEROS.
               05  WRK-5507-MSG-RETORNO
                                       PIC X(45)           VALUE SPACES.
               05  WRK-5507-IDTFD-MASTER
                                       PIC X(01)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO SENH0315   *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC X(06)           VALUE
           'VRS001'.
       01  WRK-MENSAGEM-0315.
           03 WRK-COD-MENSAGEM-0315    PIC 9(03)           VALUE ZEROS.
           03 WRK-HIFEN-MENSAGEM-0315  PIC X(01)           VALUE SPACES.
           03 WRK-TEXTO-MENSAGEM-0315  PIC X(79)           VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC X(07)           VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC X(40)           VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*     03 WRK-CNPJ-PRI-0315        PIC 9(09)           VALUE ZEROS.
ST25X6*     03 WRK-CNPJ-FIL-0315        PIC 9(05)           VALUE ZEROS.
ST25X6*     03 WRK-CNPJ-CON-0315        PIC 9(02)           VALUE ZEROS.
           03 WRK-CNPJ-PRI-0315        PIC X(09)           VALUE SPACES.
           03 WRK-CNPJ-FIL-0315        PIC X(04)           VALUE SPACES.
           03 WRK-CNPJ-CON-0315        PIC X(02)           VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-SECAO-0315              PIC 9(03)           VALUE ZEROS.
       01  WRK-STATUS-0315             PIC X(01)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB032
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               DECLARE CSR-1 CURSOR FOR
               SELECT  CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
               FROM    DB2PRD.CTRL_ESTRN_PCIAL
               WHERE  (CSERVC_TARIF      = :GFCTB032.CSERVC-TARIF
                AND    DINIC_CTRL_ESTRN >= :WRK-DATA-PONTEIRO)
                 OR   (CSERVC_TARIF      > :GFCTB032.CSERVC-TARIF)
               GROUP   BY CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
               ORDER   BY CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
           END-EXEC.

           EXEC SQL
               DECLARE CSR-2 CURSOR FOR
               SELECT  CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
               FROM    DB2PRD.CTRL_ESTRN_PCIAL
               WHERE  (CSERVC_TARIF     >= :GFCTB032.CSERVC-TARIF)
                 AND  (DINIC_CTRL_ESTRN <= :GFCTB032.DINIC-CTRL-ESTRN
                 AND   DFIM_CTRL_ESTRN  >= :GFCTB032.DFIM-CTRL-ESTRN)
                AND   (DINIC_CTRL_ESTRN >= :WRK-DATA-PONTEIRO)
               GROUP   BY CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
               ORDER   BY CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
           END-EXEC.

           EXEC SQL
               DECLARE CSR-3 CURSOR FOR
               SELECT  CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
               FROM    DB2PRD.CTRL_ESTRN_PCIAL
               WHERE  (CSERVC_TARIF     >= :GFCTB032.CSERVC-TARIF)
                 AND  (DINIC_CTRL_ESTRN <  :GFCTB032.DINIC-CTRL-ESTRN
                  OR   DINIC_CTRL_ESTRN >  :GFCTB032.DINIC-CTRL-ESTRN)
                 AND  (DINIC_CTRL_ESTRN >= :WRK-DATA-PONTEIRO)
               GROUP   BY CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
               ORDER   BY CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
           END-EXEC.

           EXEC SQL
               DECLARE CSR-4 CURSOR FOR
               SELECT  DISTINCT(CSERVC_TARIF)
               FROM    DB2PRD.CTRL_ESTRN_PCIAL
               WHERE  (CSERVC_TARIF     = :GFCTB032.CSERVC-TARIF
                 AND ((DINIC_CTRL_ESTRN <= :GFCTB032.DINIC-CTRL-ESTRN
                 AND   DFIM_CTRL_ESTRN  >= :GFCTB032.DFIM-CTRL-ESTRN)
                  OR  (DINIC_CTRL_ESTRN >  :GFCTB032.DINIC-CTRL-ESTRN)))
                  OR  (CSERVC_TARIF     > :GFCTB032.CSERVC-TARIF
                 AND ((DINIC_CTRL_ESTRN <= :GFCTB032.DINIC-CTRL-ESTRN
                 AND   DFIM_CTRL_ESTRN  >= :GFCTB032.DFIM-CTRL-ESTRN)
                  OR  (DINIC_CTRL_ESTRN > :GFCTB032.DINIC-CTRL-ESTRN)))
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-5 CURSOR FOR
               SELECT  CSERVC_TARIF,
                       DFIM_CTRL_ESTRN
               FROM    DB2PRD.CTRL_ESTRN_PCIAL
               WHERE  (CSERVC_TARIF     >= :GFCTB032.CSERVC-TARIF)
                 AND  (DFIM_CTRL_ESTRN  <  :GFCTB032.DFIM-CTRL-ESTRN)
                 AND  (DFIM_CTRL_ESTRN  >= :WRK-DATA-PONTEIRO)
               GROUP   BY CSERVC_TARIF,
                       DFIM_CTRL_ESTRN
               ORDER   BY CSERVC_TARIF,
                       DFIM_CTRL_ESTRN
           END-EXEC.

           EXEC SQL
               DECLARE CSR-6 CURSOR FOR
               SELECT  CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
               FROM    DB2PRD.CTRL_ESTRN_PCIAL
               WHERE  (CSERVC_TARIF     >= :GFCTB032.CSERVC-TARIF)
                 AND  (DINIC_CTRL_ESTRN >  :GFCTB032.DINIC-CTRL-ESTRN)
                 AND  (DINIC_CTRL_ESTRN >= :WRK-DATA-PONTEIRO)
               GROUP   BY CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
               ORDER   BY CSERVC_TARIF,
                       DINIC_CTRL_ESTRN
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5046   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC I#GFCTAA
-INC I#GFCTAB
-INC I#GFCT0M

      *================================================================*
       PROCEDURE DIVISION              USING GFCTAA-ENTRADA
                                             GFCTAB-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIALIZAR.

           PERFORM 20000-PROCESSAR.

           PERFORM 30000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           PERFORM 11000-INICIALIZA-SAIDA.

           PERFORM 12000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-INICIALIZA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTAB-SAIDA.

           MOVE +268                   TO GFCTAB-LL.
           MOVE GFCTAA-ZZ              TO GFCTAB-ZZ.
           MOVE GFCTAA-TRANSACAO       TO GFCTAB-TRANSACAO.
           MOVE GFCTAA-FUNCAO          TO GFCTAB-FUNCAO.
           MOVE GFCTAA-QTDE-OCOR       TO GFCTAB-QTDE-OCOR.
           MOVE GFCTAA-QTDE-TOT-REG    TO GFCTAB-QTDE-TOT-REG.
           MOVE GFCTAA-CSERVC-TARIF    TO GFCTAB-CSERVC-TARIF.
           MOVE GFCTAA-FUNC-BDSCO      TO GFCTAB-FUNC-BDSCO.
           MOVE GFCTAA-TERMINAL        TO GFCTAB-TERMINAL.
           MOVE GFCTAA-PONTEIRO        TO GFCTAB-PONTEIRO.
           MOVE ZEROS                  TO GFCTAB-ERRO
                                          GFCTAB-COD-SQL-ERRO
                                          GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.
           MOVE 'N'                    TO GFCTAB-FIM.

           PERFORM 11100-OBTER-DESC-MSG.

           MOVE WRK-5000-DESC-MSG      TO GFCTAB-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-DESC-MSG            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5000-AREA-RECEBE.

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCTAB-ERRO
                   MOVE ZEROS          TO GFCTAB-COD-SQL-ERRO
                                          GFCTAB-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTAB-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTAB-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTAB-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTAB-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTAB-DESC-MSG-ERRO
               END-IF
               MOVE 'S'                TO GFCTAB-FIM
               INITIALIZE GFCTAB-DADOS
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTAA-TRANSACAO        EQUAL SPACES)          OR
              (GFCTAA-FUNCAO           EQUAL SPACES)          OR
              (GFCTAA-CSERVC-TARIF     IS NOT NUMERIC)        OR
              (GFCTAA-QTDE-OCOR        IS NOT NUMERIC)        OR
              (GFCTAA-QTDE-TOT-REG     IS NOT NUMERIC)        OR
              (GFCTAA-FUNC-BDSCO       EQUAL SPACES)          OR
              (GFCTAA-PONTEIRO-TARIFA  IS NOT NUMERIC)        OR
              (GFCTAA-FIM              NOT EQUAL 'S' AND 'N') OR
              (GFCTAA-TIPO-VIGENCIA    NOT EQUAL 'V' AND 'N'  AND
                                                 'T' AND 'P'  AND
                                                 'H' AND 'F') OR
              (GFCTAA-TIPO-DATA        IS NOT NUMERIC         OR
               GFCTAA-TIPO-DATA        NOT EQUAL  0  AND  1   AND
                                                  2  AND  3 ) OR
              (GFCTAA-TIPO-DATA        EQUAL      0           AND
               GFCTAA-DATA-REFER       EQUAL SPACES )
               MOVE 1                  TO GFCTAB-ERRO
               MOVE ZEROS              TO GFCTAB-COD-SQL-ERRO
               MOVE 0001               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTAB-FIM
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTAB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 20100-OBTER-COD-JUNCAO.

           PERFORM 20200-VERIFICAR-MASTER.

           IF  GFCTAA-PONTEIRO-TARIFA  EQUAL ZEROS
               MOVE GFCTAA-CSERVC-TARIF
                                       TO WRK-AUX-TARIFA
               MOVE '01.01.0001'       TO WRK-DATA-PONTEIRO
           ELSE
               MOVE GFCTAA-PONTEIRO-TARIFA
                                       TO WRK-AUX-TARIFA
               MOVE GFCTAA-PONTEIRO-DATA
                                       TO WRK-DATA-PONTEIRO
           END-IF.

           IF  GFCTAA-TIPO-VIGENCIA    NOT EQUAL 'T'
               PERFORM 21000-FORMATAR-DT-REF
           END-IF.

           EVALUATE GFCTAA-TIPO-VIGENCIA
               WHEN 'T'
                   PERFORM 22000-TRATAR-CSR-1
               WHEN 'V'
                   PERFORM 23000-TRATAR-CSR-2
               WHEN 'N'
                   PERFORM 24000-TRATAR-CSR-3
               WHEN 'P'
                   PERFORM 25000-TRATAR-CSR-4
               WHEN 'H'
                   PERFORM 26000-TRATAR-CSR-5
               WHEN OTHER
                   PERFORM 27000-TRATAR-CSR-6
           END-EVALUATE.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20100-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCTAA-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                  TO GFCTAB-ERRO
               MOVE RETURN-CODE        TO GFCTAB-COD-SQL-ERRO
               MOVE 56                 TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTAB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               EVALUATE WRK-COD-MENSAGEM-0315
                   WHEN 1
                       MOVE 1          TO GFCTAB-ERRO
                       MOVE 57         TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 154
                       MOVE 1          TO GFCTAB-ERRO
                       MOVE 58         TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 242
                       MOVE 1          TO GFCTAB-ERRO
                       MOVE 59         TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 9
                       MOVE ZEROS      TO GFCTAB-ERRO
                                          GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                       MOVE 'S'        TO GFCTAB-FIM
                   WHEN 138
                       MOVE 1          TO GFCTAB-ERRO
                       MOVE 60         TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               END-EVALUATE
               MOVE ZEROS              TO GFCTAB-COD-SQL-ERRO
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTAB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       20100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20200-VERIFICAR-MASTER          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5507-AREA.

           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5507-AREA.

           IF  WRK-5507-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 1
                   MOVE 1              TO GFCTAB-ERRO
                   MOVE ZEROS          TO GFCTAB-COD-SQL-ERRO
                                          GFCTAB-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTAB-DESC-MSG-ERRO
               END-IF
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTAB-ERRO
                   MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCTAB-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTAB-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTAB-DESC-MSG-ERRO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       20200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-FORMATAR-DT-REF           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTAA-TIPO-DATA        EQUAL ZEROS
               MOVE GFCTAA-DATA-REFER  TO WRK-DATA-DD-MM-AAAA
               PERFORM 21100-FORMATAR-DT-WEB
               IF  WRK-ERRO-DATA       EQUAL 'S'
                   MOVE 1              TO GFCTAB-ERRO
                   MOVE ZEROS          TO GFCTAB-COD-SQL-ERRO
                   MOVE 125            TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'S'            TO GFCTAB-FIM
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTAB-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               ELSE
                   MOVE WRK-DATA-AAAA  TO WRK-DTREF-ANO
                   MOVE WRK-DATA-MM    TO WRK-DTREF-MES
                   MOVE WRK-DATA-DD    TO WRK-DTREF-DIA
               END-IF
           ELSE
               PERFORM 21200-OBTER-DT-PROCE
               IF  GFCTAA-TIPO-DATA    EQUAL 1
                   MOVE DPROCM-ANTER OF GFCTB0A1
                                       TO WRK-DATA-REFER
               ELSE
                   IF  GFCTAA-TIPO-DATA
                                       EQUAL 2
                       MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-REFER
                   ELSE
                       MOVE DPROCM-PROX OF GFCTB0A1
                                       TO WRK-DATA-REFER
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21100-FORMATAR-DT-WEB           SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ERRO-DATA.

           IF  WRK-DATA-DD-MM-AAAA     EQUAL SPACES
               MOVE 'S'                TO WRK-ERRO-DATA
           ELSE
               MOVE WRK-DATA-DD        TO WRK-CAD-DIA
               MOVE WRK-DATA-MM        TO WRK-CAD-MES
               MOVE WRK-DATA-AAAA      TO WRK-CAD-ANO
               CALL 'POOL1470'         USING WRK-CAD-DATA
               IF  RETURN-CODE         NOT EQUAL ZEROS
                   MOVE 'S'            TO WRK-ERRO-DATA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       21100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21200-OBTER-DT-PROCE            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
               SELECT  DPROCM_ANTER,
                       DPROCM_ATUAL,
                       DPROCM_PROX
               INTO    :GFCTB0A1.DPROCM-ANTER,
                       :GFCTB0A1.DPROCM-ATUAL,
                       :GFCTB0A1.DPROCM-PROX
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21210-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5046'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO GFCTAB-ERRO.

           PERFORM 11100-OBTER-DESC-MSG.

           PERFORM 21211-CONCATENAR-MSG.

      *----------------------------------------------------------------*
       21210-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21211-CONCATENAR-MSG            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTAB-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO GFCTAB-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       21211-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-TRATAR-CSR-1              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           MOVE ZEROS                  TO IND-1.

           PERFORM 22200-ABRIR-CSR-1.

           PERFORM 22300-LER-CSR-1.

           PERFORM
             UNTIL WRK-FINALIZAR       EQUAL 'S'
                OR IND-1               GREATER 7
                   MOVE 'T'            TO WRK-TIPO-VIGENCIA

                   PERFORM 22100-TRATAR-CAMPOS-CSR-1
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB032
                                       TO GFCTAB-PONTEIRO-TARIFA
               MOVE DINIC-CTRL-ESTRN OF GFCTB032
                                       TO GFCTAB-PONTEIRO-DATA
           END-IF.

           PERFORM 22400-FECHAR-CSR-1.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22100-TRATAR-CAMPOS-CSR-1       SECTION.
      *----------------------------------------------------------------*

           PERFORM 29000-OBTER-DADOS-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  22100-99-FIM
               END-IF

               PERFORM 28000-MOVER-CAMPOS-TARIFA
           END-IF.

           PERFORM 22300-LER-CSR-1.

      *----------------------------------------------------------------*
       22100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22200-ABRIR-CSR-1               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB032.

           EXEC SQL
               OPEN CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       22200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22300-LER-CSR-1                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-1
               INTO    :GFCTB032.CSERVC-TARIF,
                       :GFCTB032.DINIC-CTRL-ESTRN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 22310-TRATAR-FIM-CSR
           END-IF.

      *----------------------------------------------------------------*
       22300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22310-TRATAR-FIM-CSR            SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-FINALIZAR
                                          GFCTAB-FIM.

           IF  IND-1                   EQUAL ZEROS
               MOVE ZEROS              TO GFCTAB-ERRO
                                          GFCTAB-COD-SQL-ERRO
               MOVE 0007               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 11100-OBTER-DESC-MSG
               PERFORM 21211-CONCATENAR-MSG
           END-IF.

      *----------------------------------------------------------------*
       22310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22400-FECHAR-CSR-1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       22400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-TRATAR-CSR-2              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           MOVE ZEROS                  TO IND-1.

           PERFORM 23200-ABRIR-CSR-2.

           PERFORM 23300-LER-CSR-2.

           PERFORM
             UNTIL WRK-FINALIZAR       EQUAL 'S'
                OR IND-1               GREATER 7
                   MOVE 'V'            TO WRK-TIPO-VIGENCIA

                   PERFORM 23100-TRATAR-CAMPOS-CSR-2
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB032
                                       TO GFCTAB-PONTEIRO-TARIFA
               MOVE DINIC-CTRL-ESTRN OF GFCTB032
                                       TO GFCTAB-PONTEIRO-DATA
           END-IF.

           PERFORM 23400-FECHAR-CSR-2.

      *----------------------------------------------------------------*
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23100-TRATAR-CAMPOS-CSR-2       SECTION.
      *----------------------------------------------------------------*

           PERFORM 29000-OBTER-DADOS-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  23100-99-FIM
               END-IF

               PERFORM 28000-MOVER-CAMPOS-TARIFA
           END-IF.

           PERFORM 23300-LER-CSR-2.

      *----------------------------------------------------------------*
       23100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23200-ABRIR-CSR-2               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB032.
           MOVE WRK-DATA-REFER         TO DINIC-CTRL-ESTRN OF GFCTB032
                                          DFIM-CTRL-ESTRN  OF GFCTB032.

           EXEC SQL
               OPEN CSR-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23300-LER-CSR-2                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-2
               INTO    :GFCTB032.CSERVC-TARIF,
                       :GFCTB032.DINIC-CTRL-ESTRN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 22310-TRATAR-FIM-CSR
           END-IF.

      *----------------------------------------------------------------*
       23300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23400-FECHAR-CSR-2              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0008'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       23400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24000-TRATAR-CSR-3              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           MOVE ZEROS                  TO IND-1.

           PERFORM 24200-ABRIR-CSR-3.

           PERFORM 24300-LER-CSR-3.

           PERFORM
             UNTIL WRK-FINALIZAR       EQUAL 'S'
                OR IND-1               GREATER 7
                   MOVE SPACES         TO WRK-TIPO-VIGENCIA

                   PERFORM 24100-TRATAR-CAMPOS-CSR-3
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB032
                                       TO GFCTAB-PONTEIRO-TARIFA
               MOVE DINIC-CTRL-ESTRN OF GFCTB032
                                       TO GFCTAB-PONTEIRO-DATA
           END-IF.

           PERFORM 24400-FECHAR-CSR-3.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24100-TRATAR-CAMPOS-CSR-3       SECTION.
      *----------------------------------------------------------------*

           PERFORM 29000-OBTER-DADOS-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  24100-99-FIM
               END-IF

               PERFORM 28000-MOVER-CAMPOS-TARIFA
           END-IF.

           PERFORM 24300-LER-CSR-3.

      *----------------------------------------------------------------*
       24100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24200-ABRIR-CSR-3               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB032.
           MOVE WRK-DATA-REFER         TO DINIC-CTRL-ESTRN OF GFCTB032.

           EXEC SQL
               OPEN CSR-3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0009'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24300-LER-CSR-3                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-3
               INTO  :GFCTB032.CSERVC-TARIF,
                     :GFCTB032.DINIC-CTRL-ESTRN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 22310-TRATAR-FIM-CSR
           END-IF.

      *----------------------------------------------------------------*
       24300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24400-FECHAR-CSR-3              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0011'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       24400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25000-TRATAR-CSR-4              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           MOVE ZEROS                  TO IND-1.

           PERFORM 25200-ABRIR-CSR-4.

           PERFORM 25300-LER-CSR-4.

           PERFORM
             UNTIL WRK-FINALIZAR       EQUAL 'S'
                OR IND-1               GREATER 7
                   PERFORM 25400-INVERTER-DATAS

                   IF  WRK-DT-DINIC    GREATER WRK-DT-REFER
                       MOVE 'P'        TO WRK-TIPO-VIGENCIA
                   ELSE
                       MOVE 'V'        TO WRK-TIPO-VIGENCIA
                   END-IF

                   PERFORM 25100-TRATAR-CAMPOS-CSR-4
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB032
                                       TO GFCTAB-PONTEIRO-TARIFA
           END-IF.

           PERFORM 25500-FECHAR-CSR-4.

      *----------------------------------------------------------------*
       25000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25100-TRATAR-CAMPOS-CSR-4       SECTION.
      *----------------------------------------------------------------*

           PERFORM 29000-OBTER-DADOS-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  25100-99-FIM
               END-IF

               PERFORM 28000-MOVER-CAMPOS-TARIFA
           END-IF.

           PERFORM 25300-LER-CSR-4.

      *----------------------------------------------------------------*
       25100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25200-ABRIR-CSR-4               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB032.
           MOVE WRK-DATA-REFER         TO DINIC-CTRL-ESTRN OF GFCTB032
                                          DFIM-CTRL-ESTRN  OF GFCTB032.

           EXEC SQL
               OPEN CSR-4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0012'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25300-LER-CSR-4                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-4
               INTO :GFCTB032.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0013'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 22310-TRATAR-FIM-CSR
           END-IF.

      *----------------------------------------------------------------*
       25300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25400-INVERTER-DATAS            SECTION.
      *----------------------------------------------------------------*

           STRING DINIC-CTRL-ESTRN OF GFCTB032 (07:04)
                  DINIC-CTRL-ESTRN OF GFCTB032 (04:02)
                  DINIC-CTRL-ESTRN OF GFCTB032 (01:02)
           DELIMITED BY SIZE           INTO WRK-DT-DINIC.

           STRING WRK-DTREF-ANO
                  WRK-DTREF-MES
                  WRK-DTREF-DIA
           DELIMITED BY SIZE           INTO WRK-DT-REFER.

      *----------------------------------------------------------------*
       25400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25500-FECHAR-CSR-4              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0014'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       25500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26000-TRATAR-CSR-5              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           MOVE ZEROS                  TO IND-1.

           PERFORM 26200-ABRIR-CSR-5.

           PERFORM 26300-LER-CSR-5.

           PERFORM
             UNTIL WRK-FINALIZAR       EQUAL 'S'
                OR IND-1               GREATER 7
                   MOVE 'H'            TO WRK-TIPO-VIGENCIA

                   PERFORM 26100-TRATAR-CAMPOS-CSR-5
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB032
                                       TO GFCTAB-PONTEIRO-TARIFA
               MOVE DFIM-CTRL-ESTRN OF GFCTB032
                                       TO GFCTAB-PONTEIRO-DATA
           END-IF.

           PERFORM 26400-FECHAR-CSR-5.

      *----------------------------------------------------------------*
       26000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26100-TRATAR-CAMPOS-CSR-5       SECTION.
      *----------------------------------------------------------------*

           PERFORM 29000-OBTER-DADOS-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  26100-99-FIM
               END-IF

               PERFORM 28000-MOVER-CAMPOS-TARIFA
           END-IF.

           PERFORM 26300-LER-CSR-5.

      *----------------------------------------------------------------*
       26100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26200-ABRIR-CSR-5               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB032.
           MOVE WRK-DATA-REFER         TO DFIM-CTRL-ESTRN  OF GFCTB032.

           EXEC SQL
               OPEN CSR-5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0015'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       26200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26300-LER-CSR-5                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-5
               INTO  :GFCTB032.CSERVC-TARIF,
                     :GFCTB032.DFIM-CTRL-ESTRN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0016'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 22310-TRATAR-FIM-CSR
           END-IF.

      *----------------------------------------------------------------*
       26300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26400-FECHAR-CSR-5              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0017'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       26400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27000-TRATAR-CSR-6              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           MOVE ZEROS                  TO IND-1.

           PERFORM 27200-ABRIR-CSR-6.

           PERFORM 27300-LER-CSR-6.

           PERFORM
             UNTIL WRK-FINALIZAR       EQUAL 'S'
                OR IND-1               GREATER 7
                   MOVE 'F'            TO WRK-TIPO-VIGENCIA

                   PERFORM 27100-TRATAR-CAMPOS-CSR-6
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB032
                                       TO GFCTAB-PONTEIRO-TARIFA
               MOVE DINIC-CTRL-ESTRN OF GFCTB032
                                       TO GFCTAB-PONTEIRO-DATA
           END-IF.

           PERFORM 27400-FECHAR-CSR-6.

      *----------------------------------------------------------------*
       27000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27100-TRATAR-CAMPOS-CSR-6       SECTION.
      *----------------------------------------------------------------*

           PERFORM 29000-OBTER-DADOS-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  27100-99-FIM
               END-IF

               PERFORM 28000-MOVER-CAMPOS-TARIFA
           END-IF.

           PERFORM 27300-LER-CSR-6.

      *----------------------------------------------------------------*
       27100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27200-ABRIR-CSR-6               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB032.
           MOVE WRK-DATA-REFER         TO DINIC-CTRL-ESTRN OF GFCTB032.

           EXEC SQL
               OPEN CSR-6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0018'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       27200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27300-LER-CSR-6                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-6
               INTO  :GFCTB032.CSERVC-TARIF,
                     :GFCTB032.DINIC-CTRL-ESTRN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0019'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 22310-TRATAR-FIM-CSR
           END-IF.

      *----------------------------------------------------------------*
       27300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27400-FECHAR-CSR-6              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'CTRL_ESTRN_PCIAL' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB032'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       27400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28000-MOVER-CAMPOS-TARIFA       SECTION.
      *----------------------------------------------------------------*

           ADD 106                     TO GFCTAB-LL.

           MOVE CSERVC-TARIF           OF GFCTB0D8
                                       TO GFCTAB-COD-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTAB-DESC-TARIFA(IND-1).
           MOVE WRK-TIPO-VIGENCIA      TO GFCTAB-TIPO-VIGENCIA(IND-1).

           ADD 1                       TO GFCTAB-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       28000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29000-OBTER-DADOS-GFCTB0D8      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB032
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT  CDEPDC,
                       RSERVC_TARIF_REDZD
               INTO    :GFCTB0D8.CDEPDC,
                       :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF     = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTAB-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE 0009               TO GFCTAB-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 21210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
