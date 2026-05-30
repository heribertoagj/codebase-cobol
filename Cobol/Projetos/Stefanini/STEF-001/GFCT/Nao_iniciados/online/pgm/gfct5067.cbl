      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5067.
       AUTHOR. CARLOS EDUARDO ARDIGO.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5067                                     *
      *    PROGRAMADOR  : CARLOS EDUARDO ARDIGO         - CPM/FPOLIS   *
      *    ANALISTA CPM : SILVANA F. SOUZA MACHADO      - CPM/FPOLIS   *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK    - GRUPO 50     *
      *    DATA         : 21/07/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      OBTER DADOS PARA COMBO DE TARIFA DA HIERARQUIA PASSO      *
      *      A PASSO.                                                  *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.HIERQ_ALCAD_PASSO               GFCTB082          *
      *       DB2PRD.PARM_DATA_PROCM                 GFCTB0A1          *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0D8          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5500 - VERIFICAR ONLINE                               *
      *      GFCT5507 - VERIFICA MASTER                                *
      *      SENH0315 - OBTER JUNCAO                                   *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      POOL1470 - CONSISTIR DATAS                                *
      *      POOL7600 - OBTEM DATA E HORA DO SISTEMA                   *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7800                                     *
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
           '*  INICIO DA WORKING GFCT5067  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND                         PIC 9(03)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS                   PIC 9(13)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-DATA1                   PIC 9(08)           VALUE ZEROS.
       01  WRK-DATA2                   PIC 9(08)           VALUE ZEROS.
       01  WRK-FINALIZAR               PIC X(01)           VALUE SPACES.
       01  WRK-ERRO-DATA               PIC X(01)           VALUE SPACES.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.
       01  WRK-PONTEIRO.
           03  WRK-AUX-TARIFA          PIC 9(05)           VALUE ZEROS.
       01  WRK-DATA-PONTEIRO           PIC X(10)           VALUE SPACES.

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
           '*  AREA PARA MODULO SENH0315   *'.
      *----------------------------------------------------------------*

       01  WRK-0315-AREA.
           03  WRK-VERSAO-0315         PIC X(06)           VALUE SPACES.
           03  WRK-MENSAGEM-0315.
               05  WRK-COD-MENSAGEM-0315
                                       PIC 9(03)           VALUE ZEROS.
               05  WRK-HIFEN-MENSAGEM-0315
                                       PIC X(01)           VALUE SPACES.
               05  WRK-TEXTO-MENSAGEM-0315
                                       PIC X(79)           VALUE SPACES.
           03  WRK-COD-FUNCIONAL-0315  PIC X(07)           VALUE SPACES.
           03  WRK-NOME-FUNC-0315      PIC X(40)           VALUE SPACES.
           03  WRK-COD-BANCO-0315      PIC 9(05)           VALUE ZEROS.
           03  WRK-NOME-BANCO-0315     PIC X(40)           VALUE SPACES.
           03  WRK-CNPJ-BANCO-0315.
ST25X6*        05  WRK-CNPJ-PRI-0315   PIC 9(09)           VALUE ZEROS.
ST25X6*        05  WRK-CNPJ-FIL-0315   PIC 9(05)           VALUE ZEROS.
ST25X6*        05  WRK-CNPJ-CON-0315   PIC 9(02)           VALUE ZEROS.
               05  WRK-CNPJ-PRI-0315   PIC X(09)           VALUE SPACES.
               05  WRK-CNPJ-FIL-0315   PIC X(04)           VALUE SPACES.
               05  WRK-CNPJ-CON-0315   PIC X(02)           VALUE SPACES.
           03  WRK-COD-JUNCAO-0315     PIC 9(05)           VALUE ZEROS.
           03  WRK-NOME-DEPTO-0315     PIC X(40)           VALUE SPACES.
           03  WRK-SECAO-0315          PIC 9(03)           VALUE ZEROS.
           03  WRK-STATUS-0315         PIC X(01)           VALUE SPACES.

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
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB082
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
               DECLARE CSR-1 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_PASSO
               WHERE   CTPO_OPER_MOTVO = :GFCTB082.CTPO-OPER-MOTVO
                 AND   CSERVC_TARIF   >= :GFCTB082.CSERVC-TARIF
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-2 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_PASSO
               WHERE   CTPO_OPER_MOTVO    = :GFCTB082.CTPO-OPER-MOTVO
                 AND   CSERVC_TARIF      >= :GFCTB082.CSERVC-TARIF
                 AND   DINIC_ALCAD_PASSO <= :GFCTB082.DINIC-ALCAD-PASSO
                 AND   DFIM_ALCAD_PASSO  >= :GFCTB082.DFIM-ALCAD-PASSO
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-3 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_PASSO
               WHERE   CTPO_OPER_MOTVO = :GFCTB082.CTPO-OPER-MOTVO
                 AND   CSERVC_TARIF  >= :GFCTB082.CSERVC-TARIF
                 AND   (DINIC_ALCAD_PASSO > :GFCTB082.DINIC-ALCAD-PASSO
                  OR    DFIM_ALCAD_PASSO  < :GFCTB082.DFIM-ALCAD-PASSO)
               GROUP   BY CSERVC_TARIF
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-4 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
**************         DINIC_ALCAD_PASSO
               FROM    DB2PRD.HIERQ_ALCAD_PASSO
               WHERE   CTPO_OPER_MOTVO      = :GFCTB082.CTPO-OPER-MOTVO
                 AND   ((CSERVC_TARIF       = :GFCTB082.CSERVC-TARIF
                 AND    DINIC_ALCAD_PASSO  >= :WRK-DATA-PONTEIRO)
                  OR    CSERVC_TARIF       >  :GFCTB082.CSERVC-TARIF)
                 AND   ((DINIC_ALCAD_PASSO <=
                                             :GFCTB082.DINIC-ALCAD-PASSO
                 AND     DFIM_ALCAD_PASSO  >=
                                             :GFCTB082.DFIM-ALCAD-PASSO)
                  OR   (DINIC_ALCAD_PASSO  >
                                           :GFCTB082.DINIC-ALCAD-PASSO))
***************GROUP   BY CSERVC_TARIF,
***************           DINIC_ALCAD_PASSO
               ORDER   BY CSERVC_TARIF
***************           DINIC_ALCAD_PASSO
           END-EXEC.

           EXEC SQL
               DECLARE CSR-5 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_PASSO
               WHERE   CTPO_OPER_MOTVO   = :GFCTB082.CTPO-OPER-MOTVO
                 AND   CSERVC_TARIF     >= :GFCTB082.CSERVC-TARIF
                 AND   DFIM_ALCAD_PASSO < :GFCTB082.DFIM-ALCAD-PASSO
               GROUP   BY CSERVC_TARIF
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-6 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.HIERQ_ALCAD_PASSO
               WHERE   CTPO_OPER_MOTVO   = :GFCTB082.CTPO-OPER-MOTVO
                 AND   CSERVC_TARIF     >= :GFCTB082.CSERVC-TARIF
                 AND   DINIC_ALCAD_PASSO > :GFCTB082.DINIC-ALCAD-PASSO
               GROUP   BY CSERVC_TARIF
               ORDER   BY CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5067   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC I#GFCTCM
-INC I#GFCTCN
-INC I#GFCT0M

      *================================================================*
       PROCEDURE DIVISION              USING GFCTCM-ENTRADA
                                             GFCTCN-SAIDA
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

           MOVE SPACES                 TO GFCTCN-SAIDA.

           INITIALIZE GFCTCN-SAIDA.

           MOVE +268                   TO GFCTCN-LL.
           MOVE GFCTCM-ZZ              TO GFCTCN-ZZ.
           MOVE GFCTCM-TRANSACAO       TO GFCTCN-TRANSACAO.
           MOVE GFCTCM-FUNCAO          TO GFCTCN-FUNCAO.
           MOVE GFCTCM-FILTRO          TO GFCTCN-FILTRO.
           MOVE GFCTCM-FUNC-BDSCO      TO GFCTCN-FUNC-BDSCO.
           MOVE GFCTCM-TERMINAL        TO GFCTCN-TERMINAL.
           MOVE GFCTCM-PONTEIRO        TO GFCTCN-PONTEIRO.
           MOVE 'N'                    TO GFCTCN-FIM.

           IF  GFCTCM-QTDE-OCOR        IS NUMERIC AND
               GFCTCN-QTDE-OCOR        GREATER ZEROS
               MOVE GFCTCM-QTDE-OCOR   TO GFCTCN-QTDE-OCOR
           END-IF.

           IF  GFCTCM-QTDE-TOT-REG     IS NUMERIC AND
               GFCTCM-QTDE-TOT-REG     GREATER ZEROS
               MOVE GFCTCM-QTDE-TOT-REG
                                       TO GFCTCN-QTDE-TOT-REG
           END-IF.

           PERFORM 11100-OBTER-DESC-MSG.

           MOVE WRK-5000-DESC-MSG      TO GFCTCN-DESC-MSG-ERRO.

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
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                                          GFCTCN-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCN-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTCN-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTCN-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCN-DESC-MSG-ERRO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTCM-TRANSACAO        EQUAL SPACES)           OR
              (GFCTCM-FUNCAO           EQUAL SPACES)           OR
              (GFCTCM-CTPO-OPER-MOVTO  NOT NUMERIC)            OR
              (GFCTCM-QTDE-OCOR        NOT NUMERIC)            OR
              (GFCTCM-QTDE-TOT-REG     NOT NUMERIC)            OR
              (GFCTCM-FUNC-BDSCO       EQUAL SPACES)           OR
              (GFCTCM-PONTEIRO-TARIFA  NOT NUMERIC)            OR
              (GFCTCM-FIM              NOT EQUAL 'S' AND 'N')  OR
              (GFCTCM-TIPO-VIGENCIA    NOT EQUAL 'V' AND 'N'   AND
                                                 'T' AND 'P'   AND
                                                 'H' AND 'F')  OR
              (GFCTCM-TIPO-DATA        NOT EQUAL 0 AND 1 AND 2 AND 3)
               MOVE 1                  TO GFCTCN-ERRO
               MOVE ZEROS              TO GFCTCN-COD-SQL-ERRO
               MOVE 0001               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCN-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13000-VERIFICAR-ONLINE          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5500-AREA-SAIDA.

           MOVE 'GFCT5500'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5500-AREA-SAIDA.

           IF  WRK-5500-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 04
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                   MOVE 030            TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               END-IF
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   INITIALIZE          GFCT0M-AREA-ERROS
                   MOVE 'GFCT5067'     TO GFCT0M-PROGRAMA
                   MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'PARM_CTRL_GSTAO'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
                   MOVE '0020'         TO GFCT0M-LOCAL
                   MOVE SQLCA          TO GFCT0M-SQLCA-AREA
                   MOVE 9              TO GFCTCN-ERRO
                   MOVE 12             TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               END-IF
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCN-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       13000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-OBTER-JUNCAO.

           PERFORM 22000-VERIFICAR-MASTER.

           PERFORM 23000-FORMATAR-DT-REF.

           MOVE GFCTCM-PONTEIRO-TARIFA TO WRK-AUX-TARIFA.
           MOVE ZEROS                  TO IND ACU-LIDOS.
           MOVE SPACES                 TO WRK-FINALIZAR.

           EVALUATE GFCTCM-TIPO-VIGENCIA
               WHEN 'T'
                   PERFORM 24000-TRATAR-CSR-1
               WHEN 'V'
                   PERFORM 25000-TRATAR-CSR-2
               WHEN 'N'
                   PERFORM 26000-TRATAR-CSR-3
               WHEN 'P'
                   PERFORM 27000-TRATAR-CSR-4
               WHEN 'H'
                   PERFORM 28000-TRATAR-CSR-5
               WHEN OTHER
                   PERFORM 29000-TRATAR-CSR-6
           END-EVALUATE.

           IF  IND                     EQUAL ZEROS AND
               ACU-LIDOS               NOT EQUAL ZEROS
               MOVE 1                  TO GFCTCN-ERRO
               MOVE 0275               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO GFCTCN-COD-SQL-ERRO
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCN-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-OBTER-JUNCAO              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-0315-AREA.

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCTCM-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                  TO GFCTCN-ERRO
               MOVE RETURN-CODE        TO GFCTCN-COD-SQL-ERRO
               MOVE 56                 TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCN-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               EVALUATE WRK-MENSAGEM-0315
                   WHEN 1
                       MOVE 1          TO GFCTCN-ERRO
                       MOVE 57         TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 154
                       MOVE 1          TO GFCTCN-ERRO
                       MOVE 58         TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 242
                       MOVE 1          TO GFCTCN-ERRO
                       MOVE 59         TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 9
                       MOVE ZEROS      TO GFCTCN-ERRO
                                          GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                       MOVE 'S'        TO GFCTCN-FIM
                   WHEN 138
                       MOVE 1          TO GFCTCN-ERRO
                       MOVE 60         TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               END-EVALUATE
               MOVE ZEROS              TO GFCTCN-COD-SQL-ERRO
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCN-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-VERIFICAR-MASTER          SECTION.
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
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                                          GFCTCN-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTCN-DESC-MSG-ERRO
               END-IF
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCTCN-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTCN-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTCN-DESC-MSG-ERRO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-FORMATAR-DT-REF           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCM-TIPO-DATA        EQUAL ZEROS
               MOVE GFCTCM-DATA-REFER  TO WRK-DATA-DD-MM-AAAA
               PERFORM 23100-FORMATAR-DT-WEB
               IF  WRK-ERRO-DATA       EQUAL 'S'
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                   MOVE 125            TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCN-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               ELSE
                   MOVE WRK-DATA-AAAA  TO WRK-DTREF-ANO
                   MOVE WRK-DATA-MM    TO WRK-DTREF-MES
                   MOVE WRK-DATA-DD    TO WRK-DTREF-DIA
               END-IF
           ELSE
               PERFORM 23200-OBTER-DT-PROCE
               IF  GFCTCM-TIPO-DATA    EQUAL 1
                   MOVE DPROCM-ANTER OF GFCTB0A1
                                       TO WRK-DATA-REFER
               ELSE
                   IF  GFCTCM-TIPO-DATA
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
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23100-FORMATAR-DT-WEB           SECTION.
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
       23100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23200-OBTER-DT-PROCE            SECTION.
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
               WHERE   CSIST_PRINC = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23210-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5067'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO GFCTCN-ERRO.

           PERFORM 11100-OBTER-DESC-MSG.

           PERFORM 23211-CONCATENAR-MSG.

      *----------------------------------------------------------------*
       23210-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23211-CONCATENAR-MSG            SECTION.
      *----------------------------------------------------------------*

           STRING WRK-5000-DESC-MSG
                  WRK-NOME-TAB
           DELIMITED BY SIZE           INTO GFCTCN-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       23211-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24000-TRATAR-CSR-1              SECTION.
      *----------------------------------------------------------------*

           PERFORM 24100-ABRIR-CSR-1.

           PERFORM 24200-LER-CSR-1.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND EQUAL 7
               PERFORM 24300-TRATAR-CAMPOS-CSR1
               PERFORM 24200-LER-CSR-1
           END-PERFORM.

           IF  IND                     EQUAL 7   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB082
                                       TO GFCTCN-PONTEIRO-TARIFA
           END-IF.

           PERFORM 24400-FECHAR-CSR-1.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24100-ABRIR-CSR-1               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF    OF GFCTB082.
           MOVE GFCTCM-CTPO-OPER-MOVTO TO CTPO-OPER-MOTVO OF GFCTB082.

           EXEC SQL
               OPEN CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24200-LER-CSR-1                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-1
               INTO    :GFCTB082.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTCN-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE 0269           TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                   MOVE 'GFCTB082'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   PERFORM 23211-CONCATENAR-MSG
               END-IF
           ELSE
               ADD 1                   TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       24200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24300-TRATAR-CAMPOS-CSR1        SECTION.
      *----------------------------------------------------------------*

           PERFORM 24310-ACESSAR-GFCTB0D8

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND
               MOVE 'T'                TO GFCTCN-TIPO-VIGENCIA(IND)
               PERFORM 24320-MOVER-CAMPOS
           END-IF.

      *----------------------------------------------------------------*
       24300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24310-ACESSAR-GFCTB0D8          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB082
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT  CDEPDC,
                       CSERVC_TARIF,
                       RSERVC_TARIF_REDZD
               INTO    :GFCTB0D8.CDEPDC,
                       :GFCTB0D8.CSERVC-TARIF,
                       :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTCN-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE 0275           TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                   MOVE 'GFCTB0D8'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   PERFORM 23211-CONCATENAR-MSG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       24310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24320-MOVER-CAMPOS              SECTION.
      *----------------------------------------------------------------*

           ADD 106                     TO GFCTCN-LL.

           MOVE CSERVC-TARIF           OF GFCTB082
                                       TO GFCTCN-COD-TARIFA(IND).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTCN-DESC-TARIFA(IND).

           ADD 1                       TO GFCTCN-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       24320-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24400-FECHAR-CSR-1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25000-TRATAR-CSR-2              SECTION.
      *----------------------------------------------------------------*

           PERFORM 25100-ABRIR-CSR-2.

           PERFORM 25200-LER-CSR-2.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND EQUAL 7
               PERFORM 25300-TRATAR-CAMPOS-CSR2
               PERFORM 25200-LER-CSR-2
           END-PERFORM.

           IF  IND                     EQUAL 7   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB082
                                       TO GFCTCN-PONTEIRO-TARIFA
           END-IF.

           PERFORM 25400-FECHAR-CSR-2.

      *----------------------------------------------------------------*
       25000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25100-ABRIR-CSR-2               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB082.
           MOVE GFCTCM-CTPO-OPER-MOVTO TO CTPO-OPER-MOTVO   OF GFCTB082.
           MOVE WRK-DATA-REFER         TO DINIC-ALCAD-PASSO OF GFCTB082
                                          DFIM-ALCAD-PASSO  OF GFCTB082.

           EXEC SQL
               OPEN CSR-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25200-LER-CSR-2                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-2
               INTO    :GFCTB082.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTCN-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE 0270           TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                   MOVE 'GFCTB082'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   PERFORM 23211-CONCATENAR-MSG
               END-IF
           ELSE
               ADD 1                   TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       25200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25300-TRATAR-CAMPOS-CSR2        SECTION.
      *----------------------------------------------------------------*

           PERFORM 24310-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND
               MOVE 'V'                TO GFCTCN-TIPO-VIGENCIA(IND)
               PERFORM 24320-MOVER-CAMPOS
           END-IF.

      *----------------------------------------------------------------*
       25300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25400-FECHAR-CSR-2              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0008'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26000-TRATAR-CSR-3              SECTION.
      *----------------------------------------------------------------*

           PERFORM 26100-ABRIR-CSR-3.

           PERFORM 26200-LER-CSR-3.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND EQUAL 7
               PERFORM 26300-TRATAR-CAMPOS-CSR3
               PERFORM 26200-LER-CSR-3
           END-PERFORM.

           IF  IND                     EQUAL 7   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB082
                                       TO GFCTCN-PONTEIRO-TARIFA
           END-IF.

           PERFORM 26400-FECHAR-CSR-3.

      *----------------------------------------------------------------*
       26000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26100-ABRIR-CSR-3               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB082.
           MOVE GFCTCM-CTPO-OPER-MOVTO TO CTPO-OPER-MOTVO   OF GFCTB082.
           MOVE WRK-DATA-REFER         TO DINIC-ALCAD-PASSO OF GFCTB082
                                          DFIM-ALCAD-PASSO  OF GFCTB082.

           EXEC SQL
               OPEN CSR-3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0009'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       26100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26200-LER-CSR-3                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-3
               INTO    :GFCTB082.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTCN-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE 0271           TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                   MOVE 'GFCTB082'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   PERFORM 23211-CONCATENAR-MSG
               END-IF
           ELSE
               ADD 1                   TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       26200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26300-TRATAR-CAMPOS-CSR3        SECTION.
      *----------------------------------------------------------------*

           PERFORM 24310-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND
               MOVE ' '                TO GFCTCN-TIPO-VIGENCIA(IND)
               PERFORM 24320-MOVER-CAMPOS
           END-IF.

      *----------------------------------------------------------------*
       26300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26400-FECHAR-CSR-3              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0011'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       26400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27000-TRATAR-CSR-4              SECTION.
      *----------------------------------------------------------------*

           PERFORM 27100-ABRIR-CSR-4.

           PERFORM 27200-LER-CSR-4.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND EQUAL 7
               PERFORM 27300-TRATAR-CAMPOS-CSR4
               PERFORM 27200-LER-CSR-4
           END-PERFORM.

           IF  IND                     EQUAL 7   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB082
                                       TO GFCTCN-PONTEIRO-TARIFA
               MOVE DINIC-ALCAD-PASSO  OF GFCTB082
                                       TO GFCTCN-PONTEIRO-DATA
           END-IF.

           PERFORM 27400-FECHAR-CSR-4.

      *----------------------------------------------------------------*
       27000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27100-ABRIR-CSR-4               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB082.
           MOVE GFCTCM-CTPO-OPER-MOVTO TO CTPO-OPER-MOTVO   OF GFCTB082.
           MOVE WRK-DATA-REFER         TO DINIC-ALCAD-PASSO OF GFCTB082
                                          DFIM-ALCAD-PASSO  OF GFCTB082.

           IF GFCTCM-PONTEIRO-DATA        EQUAL SPACES
              MOVE '01.01.0001'           TO WRK-DATA-PONTEIRO
           ELSE
              MOVE GFCTCM-PONTEIRO-DATA   TO WRK-DATA-PONTEIRO
           END-IF.

           EXEC SQL
               OPEN CSR-4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0012'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       27100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27200-LER-CSR-4                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-4
               INTO    :GFCTB082.CSERVC-TARIF,
                       :GFCTB082.DINIC-ALCAD-PASSO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0013'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTCN-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE 0272           TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                   MOVE 'GFCTB082'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   PERFORM 23211-CONCATENAR-MSG
               END-IF
           ELSE
               ADD 1                   TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       27200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27300-TRATAR-CAMPOS-CSR4        SECTION.
      *----------------------------------------------------------------*

           PERFORM 24310-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND
               PERFORM 27310-COMPARAR-DATAS
               PERFORM 24320-MOVER-CAMPOS
           END-IF.

      *----------------------------------------------------------------*
       27300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27310-COMPARAR-DATAS            SECTION.
      *----------------------------------------------------------------*

           STRING DINIC-ALCAD-PASSO(07:04)
                  DINIC-ALCAD-PASSO(04:02)
                  DINIC-ALCAD-PASSO(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA1.

           STRING WRK-DATA-REFER(07:04)
                  WRK-DATA-REFER(04:02)
                  WRK-DATA-REFER(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA2.

           IF  WRK-DATA1               GREATER WRK-DATA2
               MOVE 'P'                TO GFCTCN-TIPO-VIGENCIA(IND)
           ELSE
               MOVE 'V'                TO GFCTCN-TIPO-VIGENCIA(IND)
           END-IF.

      *----------------------------------------------------------------*
       27310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27400-FECHAR-CSR-4              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0014'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       27400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28000-TRATAR-CSR-5              SECTION.
      *----------------------------------------------------------------*

           PERFORM 28100-ABRIR-CSR-5.

           PERFORM 28200-LER-CSR-5.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND EQUAL 7
               PERFORM 28300-TRATAR-CAMPOS-CSR5
               PERFORM 28200-LER-CSR-5
           END-PERFORM.

           IF  IND                     EQUAL 7   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB082
                                       TO GFCTCN-PONTEIRO-TARIFA
           END-IF.

           PERFORM 28400-FECHAR-CSR-5.

      *----------------------------------------------------------------*
       28000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28100-ABRIR-CSR-5               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB082.
           MOVE GFCTCM-CTPO-OPER-MOVTO TO CTPO-OPER-MOTVO   OF GFCTB082.
           MOVE WRK-DATA-REFER         TO DFIM-ALCAD-PASSO  OF GFCTB082.

           EXEC SQL
               OPEN CSR-5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0015'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       28100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28200-LER-CSR-5                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-5
               INTO    :GFCTB082.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0016'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTCN-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE 0273           TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                   MOVE 'GFCTB082'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   PERFORM 23211-CONCATENAR-MSG
               END-IF
           ELSE
               ADD 1                   TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       28200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28300-TRATAR-CAMPOS-CSR5        SECTION.
      *----------------------------------------------------------------*

           PERFORM 24310-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND
               MOVE 'H'                TO GFCTCN-TIPO-VIGENCIA(IND)
               PERFORM 24320-MOVER-CAMPOS
           END-IF.

      *----------------------------------------------------------------*
       28300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28400-FECHAR-CSR-5              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0017'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       28400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29000-TRATAR-CSR-6              SECTION.
      *----------------------------------------------------------------*

           PERFORM 29100-ABRIR-CSR-6.

           PERFORM 29200-LER-CSR-6.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND EQUAL 7
               PERFORM 29300-TRATAR-CAMPOS-CSR6
               PERFORM 29200-LER-CSR-6
           END-PERFORM.

           IF  IND                     EQUAL 7   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB082
                                       TO GFCTCN-PONTEIRO-TARIFA
           END-IF.

           PERFORM 29400-FECHAR-CSR-6.

      *----------------------------------------------------------------*
       29000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29100-ABRIR-CSR-6               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB082.
           MOVE GFCTCM-CTPO-OPER-MOVTO TO CTPO-OPER-MOTVO   OF GFCTB082.
           MOVE WRK-DATA-REFER         TO DINIC-ALCAD-PASSO OF GFCTB082.

           EXEC SQL
               OPEN CSR-6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0018'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29200-LER-CSR-6                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-6
               INTO    :GFCTB082.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0019'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTCN-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTCN-ERRO
                   MOVE 0274           TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTCN-COD-SQL-ERRO
                   MOVE 'GFCTB082'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   PERFORM 23211-CONCATENAR-MSG
               END-IF
           ELSE
               ADD 1                   TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       29200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29300-TRATAR-CAMPOS-CSR6        SECTION.
      *----------------------------------------------------------------*

           PERFORM 24310-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND
               MOVE 'F'                TO GFCTCN-TIPO-VIGENCIA(IND)
               PERFORM 24320-MOVER-CAMPOS
           END-IF.

      *----------------------------------------------------------------*
       29300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29400-FECHAR-CSR-6              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_ALCAD_PASSO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCN-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCN-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB082'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCN-ERRO             EQUAL 1
               MOVE 'S'                TO GFCTCN-FIM
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
