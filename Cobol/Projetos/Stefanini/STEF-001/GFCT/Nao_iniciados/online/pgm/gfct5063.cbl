      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5063.
       AUTHOR. RAQUEL SARA DE MATOS ALVES.
      *================================================================*
      *                    C P M   S I S T E M A S                     *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5063                                     *
      *    PROGRAMADOR  : RAQUEL SARA DE MATOS ALVES    - CPM/FPOLIS   *
      *    ANALISTA CPM : SILVANA F. SOUZA MACHADO      - CPM/FPOLIS   *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK    - GRUPO 50     *
      *    DATA         : 21/07/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA COMBO DE TARIFAS DO TIPO DE OPERACAO.    *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.HIERQ_ALCAD_EXCEC               GFCTB081          *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0D8          *
      *       DB2PRD.PARM_DATA_PROCM                 GFCTB0A1          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5507 - VERIFICA MASTER                                *
      *      GFCT5500 - VERIFICA ON-LINE                               *
      *      SENH0315 - OBTER JUNCAO                                   *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      POOL1470 - CONSISTE DATAS COM SECULO                      *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7081                                     *
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
           '*  INICIO DA WORKING GFCT5063  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-OCOR                    PIC 9(05)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS                   PIC 9(13)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-FINALIZAR               PIC X(01)           VALUE 'N'.
       01  WRK-ERRO-DATA               PIC X(01)           VALUE SPACES.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.
       01  WRK-DT-DINIC                PIC 9(08)           VALUE ZEROS.
       01  WRK-DT-REFER                PIC 9(08)           VALUE ZEROS.
       01  WRK-TIPO-VIGENCIA           PIC X(01)           VALUE SPACES.
       01  WRK-PONTEIRO-DATA           PIC X(10)           VALUE SPACES.

       01  WRK-AUX-PONTEIRO.
           03  WRK-AUX-TARIFA          PIC S9(05) COMP-3   VALUE ZEROS.

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
       01  WRK-DATA-REFER-R            REDEFINES
           WRK-DATA-REFER              PIC X(10).

       01  WRK-CAD-DATA.
           03  WRK-CAD-DIA             PIC 9(02)           VALUE ZEROS.
           03  WRK-CAD-MES             PIC 9(02)           VALUE ZEROS.
           03  WRK-CAD-ANO             PIC 9(04)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5500   *'.
      *----------------------------------------------------------------*

       01  WRK-5500-AREA-SAIDA.
           03  WRK-5500-COD-RETORNO    PIC 9(02)           VALUE ZEROS.
           03  WRK-5500-COD-SQL-ERRO   PIC 9(03)           VALUE ZEROS.
           03  WRK-5500-DESC-MSG       PIC X(70)           VALUE SPACES.

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
                                       PIC X(01)           VALUE 'N'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO SENH0315   *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC X(06)           VALUE
           'VRS001'.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MENSAGEM-0315   PIC 9(03)           VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC X(01)           VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC X(79)           VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC X(07)           VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC X(40)           VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*     03  WRK-CNPJ-PRI-0315       PIC 9(09)           VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-FIL-0315       PIC 9(05)           VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-CON-0315       PIC 9(02)           VALUE ZEROS.
           03  WRK-CNPJ-PRI-0315       PIC X(09)           VALUE SPACES.
           03  WRK-CNPJ-FIL-0315       PIC X(04)           VALUE SPACES.
           03  WRK-CNPJ-CON-0315       PIC X(02)           VALUE SPACES.
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
                INCLUDE GFCTB081
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               DECLARE CSR-1 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_EXCEC
               WHERE   CTPO_OPER_MOTVO    = :GFCTB081.CTPO-OPER-MOTVO
                 AND   CSERVC_TARIF      >= :WRK-AUX-TARIFA
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-2 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_EXCEC
               WHERE   CTPO_OPER_MOTVO    = :GFCTB081.CTPO-OPER-MOTVO
                 AND  (CSERVC_TARIF      >= :WRK-AUX-TARIFA)
                 AND  (DINIC_ALCAD_EXCEC <= :WRK-DATA-REFER-R
                 AND   DFIM_ALCAD_EXCEC  >= :WRK-DATA-REFER-R)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-3 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_EXCEC
               WHERE   CTPO_OPER_MOTVO   = :GFCTB081.CTPO-OPER-MOTVO
                 AND  (CSERVC_TARIF     >= :WRK-AUX-TARIFA)
                 AND  (DINIC_ALCAD_EXCEC < :WRK-DATA-REFER-R
                 OR    DINIC_ALCAD_EXCEC > :WRK-DATA-REFER-R)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-4 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_EXCEC
               WHERE ( CTPO_OPER_MOTVO   = :GFCTB081.CTPO-OPER-MOTVO
                 AND   CSERVC_TARIF     >= :WRK-AUX-TARIFA)
                 AND ((DINIC_ALCAD_EXCEC
                                        <= :WRK-DATA-REFER-R
                 AND   DFIM_ALCAD_EXCEC >= :WRK-DATA-REFER-R)
                  OR  (DINIC_ALCAD_EXCEC > :WRK-DATA-REFER-R))
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-5 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_EXCEC
               WHERE   CTPO_OPER_MOTVO   = :GFCTB081.CTPO-OPER-MOTVO
                 AND  (CSERVC_TARIF     >= :WRK-AUX-TARIFA)
                 AND  (DFIM_ALCAD_EXCEC  < :WRK-DATA-REFER-R)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-6 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_EXCEC
               WHERE   CTPO_OPER_MOTVO   = :GFCTB081.CTPO-OPER-MOTVO
                 AND  (CSERVC_TARIF     >= :WRK-AUX-TARIFA)
                 AND  (DINIC_ALCAD_EXCEC > :WRK-DATA-REFER-R)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5063   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC I#GFCTCE
-INC I#GFCTCF
-INC I#GFCT0M

      *================================================================*
       PROCEDURE DIVISION              USING GFCTCE-ENTRADA
                                             GFCTCF-SAIDA
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

           PERFORM 13000-VERIFICAR-ONLINE.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-INICIALIZA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTCF-SAIDA.

           MOVE +268                   TO GFCTCF-LL.
           MOVE GFCTCE-ZZ              TO GFCTCF-ZZ.
           MOVE GFCTCE-TRANSACAO       TO GFCTCF-TRANSACAO.
           MOVE GFCTCE-FUNCAO          TO GFCTCF-FUNCAO.
           MOVE GFCTCE-FILTRO          TO GFCTCF-FILTRO.

           IF  GFCTCE-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTCF-QTDE-OCOR
           ELSE
               MOVE GFCTCE-QTDE-OCOR   TO GFCTCF-QTDE-OCOR
           END-IF.

           IF  GFCTCE-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTCF-QTDE-TOT-REG
           ELSE
               MOVE GFCTCE-QTDE-TOT-REG
                                       TO GFCTCF-QTDE-TOT-REG
           END-IF.

           MOVE GFCTCE-FUNC-BDSCO      TO GFCTCF-FUNC-BDSCO.
           MOVE GFCTCE-TERMINAL        TO GFCTCF-TERMINAL.
           MOVE GFCTCE-PONTEIRO        TO GFCTCF-PONTEIRO.
           MOVE ZEROS                  TO GFCTCF-ERRO
                                          GFCTCF-COD-SQL-ERRO
                                          GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.
           MOVE 'N'                    TO GFCTCF-FIM.

           PERFORM 12100-OBTER-DESC-MSG.

           MOVE WRK-5000-DESC-MSG      TO GFCTCF-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTCE-TRANSACAO        EQUAL SPACES)          OR
              (GFCTCE-FUNCAO           EQUAL SPACES)          OR
              (GFCTCE-CTPO-OPER-MOVTO  IS NOT NUMERIC)        OR
              (GFCTCE-QTDE-OCOR        IS NOT NUMERIC)        OR
              (GFCTCE-QTDE-TOT-REG     IS NOT NUMERIC)        OR
              (GFCTCE-FUNC-BDSCO       EQUAL SPACES)          OR
              (GFCTCE-PONTEIRO-TARIFA  IS NOT NUMERIC)        OR
              (GFCTCE-FIM              NOT EQUAL 'S' AND 'N') OR
              (GFCTCE-TIPO-VIGENCIA    NOT EQUAL 'V' AND 'N'  AND
                                                 'T' AND 'P'  AND
                                                 'H' AND 'F') OR
              (GFCTCE-TIPO-DATA        IS NOT NUMERIC)        OR
              (GFCTCE-TIPO-DATA        NOT EQUAL  0  AND  1   AND
                                                  2  AND  3)  OR
              (GFCTCE-TIPO-DATA        EQUAL 0                AND
               GFCTCE-DATA-REFER       EQUAL SPACES)
               MOVE 1                  TO GFCTCF-ERRO
               MOVE ZEROS              TO GFCTCF-COD-SQL-ERRO
               MOVE 0001               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTCF-FIM
               PERFORM 12100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCF-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12100-OBTER-DESC-MSG            SECTION.
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
                   MOVE 1              TO GFCTCF-ERRO
                   MOVE ZEROS          TO GFCTCF-COD-SQL-ERRO
                                          GFCTCF-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCF-DESC-MSG-ERRO
                   MOVE 'S'            TO GFCTCF-FIM
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTCF-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTCF-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTCF-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCF-DESC-MSG-ERRO
                   MOVE 'S'            TO GFCTCF-FIM
               END-IF
               INITIALIZE GFCTCF-DADOS
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       12100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13000-VERIFICAR-ONLINE          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5500-AREA-SAIDA.

           MOVE 'GFCT5500'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5500-AREA-SAIDA.

           IF  WRK-5500-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 04
                   MOVE 1              TO GFCTCF-ERRO
                   MOVE ZEROS          TO GFCTCF-COD-SQL-ERRO
                   MOVE 030            TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'S'            TO GFCTCF-FIM
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCF-DESC-MSG-ERRO
               END-IF
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   INITIALIZE          GFCT0M-AREA-ERROS
                   MOVE 'PARM_CTRL_GSTAO'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
                   MOVE '0001'         TO GFCT0M-LOCAL
                   MOVE SQLCA          TO GFCT0M-SQLCA-AREA
                   MOVE 9              TO GFCTCF-ERRO
                   MOVE 12             TO GFCTCF-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
                   MOVE 'GFCTB0A0'     TO WRK-NOME-TAB
                   PERFORM 13100-MOVER-ERRO-SQL
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       13000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13100-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5063'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO GFCTCF-ERRO.

           PERFORM 12100-OBTER-DESC-MSG.

           MOVE SPACES                 TO GFCTCF-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO GFCTCF-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       13100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-OBTER-COD-JUNCAO.

           PERFORM 22000-VERIFICAR-MASTER.

           IF  GFCTCE-TIPO-DATA        EQUAL ZEROS
               MOVE GFCTCE-DATA-REFER  TO WRK-DATA-DD-MM-AAAA
               PERFORM 23000-CONSISTIR-DATA-WEB
               IF  WRK-ERRO-DATA       EQUAL 'S'
                   MOVE 1              TO GFCTCF-ERRO
                   MOVE ZEROS          TO GFCTCF-COD-SQL-ERRO
                   MOVE 125            TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'S'            TO GFCTCF-FIM
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCF-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               ELSE
                   MOVE WRK-DATA-AAAA  TO WRK-DTREF-ANO
                   MOVE WRK-DATA-MM    TO WRK-DTREF-MES
                   MOVE WRK-DATA-DD    TO WRK-DTREF-DIA
               END-IF
           ELSE
               PERFORM 24000-OBTER-DT-PROCESSAMENTO
               IF  GFCTCE-TIPO-DATA    EQUAL 1
                   MOVE DPROCM-ANTER OF GFCTB0A1
                                       TO WRK-DATA-REFER
               ELSE
                   IF  GFCTCE-TIPO-DATA
                                       EQUAL 2
                       MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-REFER
                   ELSE
                       MOVE DPROCM-PROX OF GFCTB0A1
                                       TO WRK-DATA-REFER
                   END-IF
               END-IF
           END-IF.

           MOVE GFCTCE-PONTEIRO-TARIFA TO WRK-AUX-TARIFA.

           IF GFCTCE-PONTEIRO-DATA     EQUAL SPACES
              MOVE '01.01.0001'        TO GFCTCE-PONTEIRO-DATA
           END-IF.

           MOVE GFCTCE-CTPO-OPER-MOVTO TO CTPO-OPER-MOTVO OF GFCTB081.
           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 25000-TRATAR-CURSORES.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCTCE-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                  TO GFCTCF-ERRO
               MOVE RETURN-CODE        TO GFCTCF-COD-SQL-ERRO
               MOVE 0056               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTCF-FIM
               PERFORM 12100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCF-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 001
                   MOVE 1              TO GFCTCF-ERRO
                   MOVE ZEROS          TO GFCTCF-COD-SQL-ERRO
                   MOVE 0057           TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'S'            TO GFCTCF-FIM
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCF-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 154
                   MOVE 1              TO GFCTCF-ERRO
                   MOVE ZEROS          TO GFCTCF-COD-SQL-ERRO
                   MOVE 0058           TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'S'            TO GFCTCF-FIM
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCF-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 242
                   MOVE 1              TO GFCTCF-ERRO
                   MOVE ZEROS          TO GFCTCF-COD-SQL-ERRO
                   MOVE 0059           TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'S'            TO GFCTCF-FIM
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCF-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 009
                   MOVE ZEROS          TO GFCTCF-ERRO
                                          GFCTCF-COD-SQL-ERRO
                                          GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCF-DESC-MSG-ERRO
                   MOVE 'S'            TO GFCTCF-FIM
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 138
                   MOVE 1              TO GFCTCF-ERRO
                   MOVE ZEROS          TO GFCTCF-COD-SQL-ERRO
                   MOVE 0060           TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'S'            TO GFCTCF-FIM
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCF-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-VERIFICAR-MASTER          SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5507-AREA.

           IF  WRK-5507-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCTCF-ERRO
                   MOVE ZEROS          TO GFCTCF-COD-SQL-ERRO
                                          GFCTCF-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTCF-DESC-MSG-ERRO
                   MOVE 'S'            TO GFCTCF-FIM
               ELSE
                   IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                       MOVE 1          TO GFCTCF-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCTCF-COD-SQL-ERRO
                       MOVE ZEROS      TO GFCTCF-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTCF-DESC-MSG-ERRO
                       MOVE 'S'        TO GFCTCF-FIM
                   END-IF
               END-IF
               INITIALIZE GFCTCF-DADOS
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-CONSISTIR-DATA-WEB        SECTION.
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
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24000-OBTER-DT-PROCESSAMENTO    SECTION.
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
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25000-TRATAR-CURSORES           SECTION.
      *----------------------------------------------------------------*

           MOVE  1                     TO IND-OCOR.
           MOVE  ZEROS                 TO ACU-LIDOS.

           EVALUATE GFCTCE-TIPO-VIGENCIA
               WHEN 'T'
                   PERFORM 25100-TRATAR-CSR-1
               WHEN 'V'
                   PERFORM 25200-TRATAR-CSR-2
               WHEN 'N'
                   PERFORM 25300-TRATAR-CSR-3
               WHEN 'P'
                   PERFORM 25400-TRATAR-CSR-4
               WHEN 'H'
                   PERFORM 25500-TRATAR-CSR-5
               WHEN OTHER
                   PERFORM 25600-TRATAR-CSR-6
           END-EVALUATE.

           IF  IND-OCOR                EQUAL 1    AND
               ACU-LIDOS               NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTCF-FIM
               MOVE 1                  TO GFCTCF-ERRO
               MOVE ZEROS              TO GFCTCF-COD-SQL-ERRO
               MOVE 0262               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 12100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCF-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       25000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25100-TRATAR-CSR-1              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25110-ABRIR-CSR-1.

           PERFORM 25120-LER-CSR-1.

           PERFORM                     UNTIL IND-OCOR GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'T'                TO WRK-TIPO-VIGENCIA
               PERFORM 25130-TRATAR-REGISTRO
               PERFORM 25120-LER-CSR-1
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               IF  IND-OCOR            GREATER 7
                   MOVE CSERVC-TARIF OF GFCTB081
                                       TO GFCTCF-PONTEIRO-TARIFA
                   MOVE 'S'            TO WRK-FINALIZAR
               END-IF
           END-IF.

           PERFORM 25140-FECHAR-CSR-1.

      *----------------------------------------------------------------*
       25100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25110-ABRIR-CSR-1               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25120-LER-CSR-1                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-1
               INTO :GFCTB081.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 25121-TRATAR-FIM-CSR1
           ELSE
               ADD     1               TO  ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       25120-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25121-TRATAR-FIM-CSR1           SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-FINALIZAR
                                          GFCTCF-FIM.

           IF  IND-OCOR                EQUAL 1
               MOVE 1                  TO GFCTCF-ERRO
               MOVE ZEROS              TO GFCTCF-COD-SQL-ERRO
               MOVE 0249               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTCF-FIM
               PERFORM 12100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCF-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       25121-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25130-TRATAR-REGISTRO           SECTION.
      *----------------------------------------------------------------*

           PERFORM 25131-ACESSAR-GFCTB0D8.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'S' OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N' AND
               CDEPDC OF GFCTB0D8      EQUAL WRK-COD-JUNCAO-0315)
               ADD 106                 TO GFCTCF-LL
               MOVE CSERVC-TARIF       OF GFCTB081
                                       TO GFCTCF-COD-TARIFA(IND-OCOR)
               MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTCF-DESC-TARIFA(IND-OCOR)
               MOVE WRK-TIPO-VIGENCIA  TO GFCTCF-TIPO-VIGENCIA(IND-OCOR)
               ADD 1                   TO GFCTCF-QTDE-TOT-REG
                                          IND-OCOR
           END-IF.

      *----------------------------------------------------------------*
       25130-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25131-ACESSAR-GFCTB0D8          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB081
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT  CSERVC_TARIF,
                       RSERVC_TARIF_REDZD,
                       CDEPDC
               INTO    :GFCTB0D8.CSERVC-TARIF,
                       :GFCTB0D8.RSERVC-TARIF-REDZD,
                       :GFCTB0D8.CDEPDC
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF     = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE 0009               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25131-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25140-FECHAR-CSR-1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25140-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25200-TRATAR-CSR-2              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25210-ABRIR-CSR-2.

           PERFORM 25220-LER-CSR-2.

           PERFORM                     UNTIL IND-OCOR GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'V'                TO WRK-TIPO-VIGENCIA
               PERFORM 25130-TRATAR-REGISTRO
               PERFORM 25220-LER-CSR-2
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               IF  IND-OCOR            GREATER 7
                   MOVE CSERVC-TARIF OF GFCTB081
                                       TO GFCTCF-PONTEIRO-TARIFA
                   MOVE 'S'            TO WRK-FINALIZAR
               END-IF
           END-IF.

           PERFORM 25230-FECHAR-CSR-2.

      *----------------------------------------------------------------*
       25200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25210-ABRIR-CSR-2               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25210-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25220-LER-CSR-2                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH  CSR-2
               INTO  :GFCTB081.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0008'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 25221-TRATAR-FIM-CSR2
           ELSE
               ADD     1               TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       25220-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25221-TRATAR-FIM-CSR2           SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-FINALIZAR
                                          GFCTCF-FIM.

           IF  IND-OCOR                EQUAL 1
               MOVE 1                  TO GFCTCF-ERRO
               MOVE ZEROS              TO GFCTCF-COD-SQL-ERRO
               MOVE 0250               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTCF-FIM
               PERFORM 12100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCF-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       25221-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25230-FECHAR-CSR-2              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0009'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25230-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25300-TRATAR-CSR-3              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25310-ABRIR-CSR-3.

           PERFORM 25320-LER-CSR-3.

           PERFORM                     UNTIL IND-OCOR GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE ' '                TO WRK-TIPO-VIGENCIA
               PERFORM 25130-TRATAR-REGISTRO
               PERFORM 25320-LER-CSR-3
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB081
                                       TO GFCTCF-PONTEIRO-TARIFA
           END-IF.

           PERFORM 25330-FECHAR-CSR-3.

      *----------------------------------------------------------------*
       25300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25310-ABRIR-CSR-3               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-REFER-R       TO DINIC-ALCAD-EXCEC OF GFCTB081.

           EXEC SQL
               OPEN CSR-3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25320-LER-CSR-3                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-3
               INTO :GFCTB081.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0011'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 25321-TRATAR-FIM-CSR3
           ELSE
               ADD     1               TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       25320-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25321-TRATAR-FIM-CSR3           SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-FINALIZAR
                                          GFCTCF-FIM.

           IF  IND-OCOR                EQUAL 1
               MOVE 1                  TO GFCTCF-ERRO
               MOVE ZEROS              TO GFCTCF-COD-SQL-ERRO
               MOVE 0251               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTCF-FIM
               PERFORM 12100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCF-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       25321-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25330-FECHAR-CSR-3              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0012'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25330-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25400-TRATAR-CSR-4              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25410-ABRIR-CSR-4.

           PERFORM 25420-LER-CSR-4.

           PERFORM                     UNTIL IND-OCOR GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               PERFORM 25300-INVERTER-DATAS
               IF  WRK-DT-DINIC        GREATER WRK-DT-REFER
                   MOVE 'P'            TO WRK-TIPO-VIGENCIA
               ELSE
                   MOVE 'V'            TO WRK-TIPO-VIGENCIA
               END-IF
               PERFORM 25130-TRATAR-REGISTRO
               PERFORM 25420-LER-CSR-4
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB081
                                       TO GFCTCF-PONTEIRO-TARIFA
               MOVE DINIC-ALCAD-EXCEC OF GFCTB081
                                       TO GFCTCF-PONTEIRO-DATA
           END-IF.

           PERFORM 25430-FECHAR-CSR-4.

      *----------------------------------------------------------------*
       25400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25410-ABRIR-CSR-4               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTCE-PONTEIRO-DATA   TO WRK-PONTEIRO-DATA.

           EXEC SQL
               OPEN CSR-4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0013'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25410-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25420-LER-CSR-4                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-4
               INTO :GFCTB081.CSERVC-TARIF,
                    :GFCTB081.DINIC-ALCAD-EXCEC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0014'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 25421-TRATAR-FIM-CSR4
           ELSE
               ADD     1               TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       25420-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25421-TRATAR-FIM-CSR4           SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-FINALIZAR
                                          GFCTCF-FIM.

           IF  IND-OCOR                EQUAL 1
               MOVE 1                  TO GFCTCF-ERRO
               MOVE ZEROS              TO GFCTCF-COD-SQL-ERRO
               MOVE 0252               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTCF-FIM
               PERFORM 12100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCF-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       25421-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25300-INVERTER-DATAS            SECTION.
      *----------------------------------------------------------------*

           STRING DINIC-ALCAD-EXCEC OF GFCTB081 (07:04)
                  DINIC-ALCAD-EXCEC OF GFCTB081 (04:02)
                  DINIC-ALCAD-EXCEC OF GFCTB081 (01:02)
           DELIMITED BY SIZE           INTO WRK-DT-DINIC.

           STRING WRK-DTREF-ANO
                  WRK-DTREF-MES
                  WRK-DTREF-DIA
           DELIMITED BY SIZE           INTO WRK-DT-REFER.

      *----------------------------------------------------------------*
       25300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25430-FECHAR-CSR-4              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0015'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25430-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25500-TRATAR-CSR-5              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25510-ABRIR-CSR-5.

           PERFORM 25520-LER-CSR-5.

           PERFORM                     UNTIL IND-OCOR GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'H'                TO WRK-TIPO-VIGENCIA
               PERFORM 25130-TRATAR-REGISTRO
               PERFORM 25520-LER-CSR-5
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB081
                                       TO GFCTCF-PONTEIRO-TARIFA
           END-IF.

           PERFORM 25530-FECHAR-CSR-5.

      *----------------------------------------------------------------*
       25500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25510-ABRIR-CSR-5               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-REFER-R       TO DFIM-ALCAD-EXCEC OF GFCTB081.

           EXEC SQL
               OPEN CSR-5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0016'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25510-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25520-LER-CSR-5                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-5
               INTO :GFCTB081.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0017'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 25521-TRATAR-FIM-CSR5
           ELSE
               ADD     1               TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       25520-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25521-TRATAR-FIM-CSR5           SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-FINALIZAR
                                          GFCTCF-FIM.

           IF  IND-OCOR                EQUAL 1
               MOVE 1                  TO GFCTCF-ERRO
               MOVE ZEROS              TO GFCTCF-COD-SQL-ERRO
               MOVE 0253               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTCF-FIM
               PERFORM 12100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCF-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       25521-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25530-FECHAR-CSR-5              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0018'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25530-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25600-TRATAR-CSR-6              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 25610-ABRIR-CSR-6.

           PERFORM 25620-LER-CSR-6.

           PERFORM                     UNTIL IND-OCOR GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'F'                TO WRK-TIPO-VIGENCIA
               PERFORM 25130-TRATAR-REGISTRO
               PERFORM 25620-LER-CSR-6
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB081
                                       TO GFCTCF-PONTEIRO-TARIFA
           END-IF.

           PERFORM 25630-FECHAR-CSR-6.

      *----------------------------------------------------------------*
       25600-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25610-ABRIR-CSR-6               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-REFER-R       TO DINIC-ALCAD-EXCEC OF GFCTB081.

           EXEC SQL
               OPEN CSR-6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0019'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25610-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25620-LER-CSR-6                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-6
               INTO :GFCTB081.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 25621-TRATAR-FIM-CSR6
           ELSE
               ADD     1               TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       25620-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25621-TRATAR-FIM-CSR6           SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-FINALIZAR
                                          GFCTCF-FIM.

           IF  IND-OCOR                EQUAL 1
               MOVE 1                  TO GFCTCF-ERRO
               MOVE ZEROS              TO GFCTCF-COD-SQL-ERRO
               MOVE 0254               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTCF-FIM
               PERFORM 12100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCF-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       25621-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25630-FECHAR-CSR-6              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCF-COD-SQL-ERRO
               MOVE '0021'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCF-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB081'         TO WRK-NOME-TAB
               PERFORM 13100-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25630-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
