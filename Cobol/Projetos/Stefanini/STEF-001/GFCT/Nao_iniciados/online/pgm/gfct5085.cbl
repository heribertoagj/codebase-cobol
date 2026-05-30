      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5085.
       AUTHOR. FERNANDO SCHUTZ.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5085                                     *
      *    PROGRAMADOR  : FERNANDO SCHUTZ               - CPM/FPOLIS   *
      *    ANALISTA CPM : SILVANA FERREIRA              - CPM/FPOLIS   *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK    - GRUPO 50     *
      *    DATA         : 08/09/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA COMBO DE TARIFA.                         *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.HIERQ_AGPTO_PPRIO               GFCTB080          *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0D8          *
      *       DB2PRD.PARM_DATA_PROCM                 GFCTB0A1          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5500 - VERIFICAR ONLINE                               *
      *      GFCT5507 - VERIFICA MASTER                                *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      POOL1470 - CONSISTE DATAS COM SECULO                      *
      *      SENH0315 - OBTER JUNCAO                                   *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7310                                     *
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
           '*  INICIO DA WORKING GFCT5085  *'.
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

       01  ACU-LIDOS                   PIC 9(09) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-FINALIZAR               PIC X(01)           VALUE SPACES.
       01  WRK-ERRO-DATA               PIC X(01)           VALUE SPACES.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.
       01  WRK-DATA1                   PIC 9(08)           VALUE ZEROS.
       01  WRK-DATA2                   PIC 9(08)           VALUE ZEROS.
       01  WRK-PONTEIRO-DATA           PIC X(10)           VALUE SPACES.
       01  WRK-AUX-PONTEIRO.
           03  WRK-AUX-TARIFA          PIC 9(05)           VALUE ZEROS.

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
            '*    AREA DO MODULO GFCT5500   *'.
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
                INCLUDE GFCTB080
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
             FROM    DB2PRD.HIERQ_AGPTO_PPRIO
             WHERE   CSERVC_TARIF      >= :GFCTB080.CSERVC-TARIF
             ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR-2 CURSOR FOR
             SELECT  DISTINCT CSERVC_TARIF
             FROM    DB2PRD.HIERQ_AGPTO_PPRIO
             WHERE   (CSERVC_TARIF     >= :GFCTB080.CSERVC-TARIF)
               AND  (DINIC_ORD_AGPTO   <= :GFCTB080.DINIC-ORD-AGPTO
               AND   DFIM_ORD_AGPTO    >= :GFCTB080.DFIM-ORD-AGPTO)
             ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR-3 CURSOR FOR
             SELECT  DISTINCT CSERVC_TARIF
             FROM    DB2PRD.HIERQ_AGPTO_PPRIO
             WHERE   (CSERVC_TARIF     >= :GFCTB080.CSERVC-TARIF)
               AND  (DINIC_ORD_AGPTO   <  :GFCTB080.DINIC-ORD-AGPTO
               AND   DFIM_ORD_AGPTO    >  :GFCTB080.DFIM-ORD-AGPTO)
             ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR-4 CURSOR FOR
             SELECT  DISTINCT CSERVC_TARIF
             FROM    DB2PRD.HIERQ_AGPTO_PPRIO
             WHERE  ((CSERVC_TARIF     >= :GFCTB080.CSERVC-TARIF)
               AND   (DINIC_ORD_AGPTO  <= :GFCTB080.DINIC-ORD-AGPTO
               AND   DFIM_ORD_AGPTO    >= :GFCTB080.DFIM-ORD-AGPTO)
                OR  (DINIC_ORD_AGPTO   >  :GFCTB080.DINIC-ORD-AGPTO))
                OR  (CSERVC_TARIF      >  :GFCTB080.CSERVC-TARIF)
               AND  ((DINIC_ORD_AGPTO  <= :GFCTB080.DINIC-ORD-AGPTO
               AND   DFIM_ORD_AGPTO    >= :GFCTB080.DFIM-ORD-AGPTO)
                OR  (DINIC_ORD_AGPTO   >  :GFCTB080.DINIC-ORD-AGPTO))
             ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR-5 CURSOR FOR
             SELECT  DISTINCT CSERVC_TARIF
             FROM    DB2PRD.HIERQ_AGPTO_PPRIO
             WHERE  (CSERVC_TARIF      >= :GFCTB080.CSERVC-TARIF)
               AND  (DFIM_ORD_AGPTO    <  :GFCTB080.DFIM-ORD-AGPTO)
             ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR-6 CURSOR FOR
             SELECT  CSERVC_TARIF
             FROM    DB2PRD.HIERQ_AGPTO_PPRIO
             WHERE  (CSERVC_TARIF      >= :GFCTB080.CSERVC-TARIF)
               AND  (DINIC_ORD_AGPTO   >  :GFCTB080.DINIC-ORD-AGPTO)
             ORDER   BY CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT5085   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC I#GFCTFH

-INC I#GFCTFI

-INC I#GFCT0M

      *================================================================*
       PROCEDURE DIVISION              USING GFCTFH-ENTRADA
                                             GFCTFI-SAIDA
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

           MOVE SPACES                 TO GFCTFI-SAIDA.

           INITIALIZE GFCTFI-SAIDA.

           MOVE 254                    TO GFCTFI-LL.
           MOVE GFCTFH-ZZ              TO GFCTFI-ZZ.
           MOVE GFCTFH-TRANSACAO       TO GFCTFI-TRANSACAO.
           MOVE GFCTFH-FUNCAO          TO GFCTFI-FUNCAO.
           MOVE GFCTFH-FUNC-BDSCO      TO GFCTFI-FUNC-BDSCO.
           MOVE GFCTFH-TERMINAL        TO GFCTFI-TERMINAL.
           MOVE GFCTFH-PONTEIRO-TARIFA TO GFCTFI-PONTEIRO-TARIFA.
           MOVE GFCTFH-PONTEIRO-DATA   TO GFCTFI-PONTEIRO-DATA.
           MOVE 'N'                    TO GFCTFI-FIM.
           MOVE ZEROS                  TO GFCTFI-ERRO
                                          GFCTFI-COD-SQL-ERRO
                                          GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           IF  GFCTFH-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTFI-QTDE-OCOR
           ELSE
               MOVE GFCTFH-QTDE-OCOR   TO GFCTFI-QTDE-OCOR
           END-IF.

           IF  GFCTFH-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTFI-QTDE-TOT-REG
           ELSE
               MOVE GFCTFH-QTDE-TOT-REG
                                       TO GFCTFI-QTDE-TOT-REG
           END-IF.

           PERFORM 11100-OBTER-DESC-MSG.

           MOVE WRK-5000-DESC-MSG      TO GFCTFI-DESC-MSG-ERRO.

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
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE ZEROS          TO GFCTFI-COD-SQL-ERRO
                                          GFCTFI-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFI-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTFI-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTFI-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFI-DESC-MSG-ERRO
               END-IF
               INITIALIZE GFCTFI-DADOS
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           PERFORM 12100-VERIFICAR-ONLINE.

           IF (GFCTFH-TRANSACAO        EQUAL SPACES)          OR
              (GFCTFH-FUNCAO           EQUAL SPACES)          OR
              (GFCTFH-QTDE-OCOR        IS NOT NUMERIC)        OR
              (GFCTFH-QTDE-TOT-REG     IS NOT NUMERIC)        OR
              (GFCTFH-FUNC-BDSCO       EQUAL SPACES)          OR
              (GFCTFH-PONTEIRO-TARIFA  IS NOT NUMERIC)        OR
              (GFCTFH-TIPO-DATA        EQUAL 0                AND
               GFCTFH-PONTEIRO-DATA    EQUAL SPACES)          OR
              (GFCTFH-FIM              NOT EQUAL 'S' AND 'N') OR
              (GFCTFH-TIPO-VIGENCIA    NOT EQUAL 'V' AND 'N'  AND
                                                 'T' AND 'P'  AND
                                                 'H' AND 'F') OR
              (GFCTFH-TIPO-DATA        IS NOT NUMERIC         OR
               GFCTFH-TIPO-DATA        NOT EQUAL  0  AND  1   AND
                                                  2  AND  3 )
               MOVE 1                  TO GFCTFI-ERRO
               MOVE ZEROS              TO GFCTFI-COD-SQL-ERRO
               MOVE 0001               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTFI-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12100-VERIFICAR-ONLINE          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5500-AREA-SAIDA.

           MOVE 'GFCT5500'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5500-AREA-SAIDA.

           IF  WRK-5500-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 04
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE ZEROS          TO GFCTFI-COD-SQL-ERRO
                   MOVE 030            TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFI-DESC-MSG-ERRO
               END-IF
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   INITIALIZE          GFCT0M-AREA-ERROS
                   MOVE 'PARM_CTRL_GSTAO'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
                   MOVE '0001'         TO GFCT0M-LOCAL
                   MOVE 12             TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'GFCTB0A0'     TO WRK-NOME-TAB
                   PERFORM 12110-MOVER-ERRO-SQL
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       12100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12110-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5085'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO GFCTFI-ERRO.

           PERFORM 11100-OBTER-DESC-MSG.

           PERFORM 12111-CONCATENAR-MSG.

      *----------------------------------------------------------------*
       12110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12111-CONCATENAR-MSG            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTFI-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO GFCTFI-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       12111-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-OBTER-COD-JUNCAO.

           PERFORM 22000-VERIFICAR-MASTER.

           PERFORM 23000-FORMATAR-DT-REF.

           MOVE GFCTFH-PONTEIRO-TARIFA TO WRK-AUX-TARIFA.
           MOVE ZEROS                  TO IND
                                          ACU-LIDOS.
           MOVE SPACES                 TO WRK-FINALIZAR.

           EVALUATE GFCTFH-TIPO-VIGENCIA
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
               MOVE 1                  TO GFCTFI-ERRO
               MOVE 0242               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO GFCTFI-COD-SQL-ERRO
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTFI-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCTFH-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                  TO GFCTFI-ERRO
               MOVE RETURN-CODE        TO GFCTFI-COD-SQL-ERRO
               MOVE 56                 TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTFI-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               EVALUATE WRK-COD-MENSAGEM-0315
                   WHEN 1
                       MOVE 1          TO GFCTFI-ERRO
                       MOVE 57         TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 154
                       MOVE 1          TO GFCTFI-ERRO
                       MOVE 58         TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 242
                       MOVE 1          TO GFCTFI-ERRO
                       MOVE 59         TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 9
                       MOVE ZEROS      TO GFCTFI-ERRO
                                          GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                       MOVE 'S'        TO GFCTFI-FIM
                   WHEN 138
                       MOVE 1          TO GFCTFI-ERRO
                       MOVE 60         TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               END-EVALUATE
               MOVE ZEROS              TO GFCTFI-COD-SQL-ERRO
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTFI-DESC-MSG-ERRO
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
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE ZEROS          TO GFCTFI-COD-SQL-ERRO
                                          GFCTFI-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTFI-DESC-MSG-ERRO
               END-IF
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCTFI-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTFI-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTFI-DESC-MSG-ERRO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-FORMATAR-DT-REF           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTFH-TIPO-DATA        EQUAL ZEROS
               MOVE GFCTFH-DATA-REFER  TO WRK-DATA-DD-MM-AAAA
               PERFORM 23100-FORMATAR-DT-WEB
               IF  WRK-ERRO-DATA       EQUAL 'S'
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE ZEROS          TO GFCTFI-COD-SQL-ERRO
                   MOVE 125            TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFI-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               ELSE
                   MOVE WRK-DATA-AAAA  TO WRK-DTREF-ANO
                   MOVE WRK-DATA-MM    TO WRK-DTREF-MES
                   MOVE WRK-DATA-DD    TO WRK-DTREF-DIA
               END-IF
           ELSE
               PERFORM 23200-OBTER-DT-PROCE
               IF  GFCTFH-TIPO-DATA    EQUAL 1
                   MOVE DPROCM-ANTER OF GFCTB0A1
                                       TO WRK-DATA-REFER
               ELSE
                   IF  GFCTFH-TIPO-DATA
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
               SELECT DPROCM_ANTER,
                      DPROCM_ATUAL,
                      DPROCM_PROX
               INTO   :GFCTB0A1.DPROCM-ANTER,
                      :GFCTB0A1.DPROCM-ATUAL,
                      :GFCTB0A1.DPROCM-PROX
               FROM   DB2PRD.PARM_DATA_PROCM
               WHERE  CSIST_PRINC      = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24000-TRATAR-CSR-1              SECTION.
      *----------------------------------------------------------------*

           PERFORM 24100-ABRIR-CSR-1.

           PERFORM 24200-LER-CSR-1.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND EQUAL 6
               PERFORM 24300-TRATAR-CAMPOS-CSR1
               PERFORM 24200-LER-CSR-1
           END-PERFORM.

           IF  IND                     EQUAL 6   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB080
                                       TO GFCTFI-PONTEIRO-TARIFA
           END-IF.

           PERFORM 24400-FECHAR-CSR-1.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24100-ABRIR-CSR-1               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF    OF GFCTB080.

           EXEC SQL
               OPEN CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
               INTO    :GFCTB080.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFI-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE 0425           TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTFI-COD-SQL-ERRO
                   MOVE 'GFCTB080'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFI-DESC-MSG-ERRO
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
               MOVE 'T'                TO GFCTFI-TIPO-VIGENCIA(IND)
               PERFORM 24320-MOVER-CAMPOS
           END-IF.

      *----------------------------------------------------------------*
       24300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24310-ACESSAR-GFCTB0D8          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB080
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
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24320-MOVER-CAMPOS              SECTION.
      *----------------------------------------------------------------*

           ADD 126                     TO GFCTFI-LL.

           MOVE CSERVC-TARIF           OF GFCTB080
                                       TO GFCTFI-COD-TARIFA(IND).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTFI-DESC-TARIFA(IND).

           PERFORM 24321-OBTER-DATA-FIM.

           MOVE DFIM-ORD-AGPTO         OF GFCTB080
                                       TO GFCTFI-DFIM-ORD-AGPTO(IND).

           PERFORM 24322-OBTER-DATA-INICIO.

           MOVE DINIC-ORD-AGPTO        OF GFCTB080
                                       TO GFCTFI-DINIC-ORD-AGPTO(IND).

           ADD 1                       TO GFCTFI-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       24320-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24321-OBTER-DATA-FIM            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFI-COD-TARIFA(IND) TO CSERVC-TARIF OF GFCTB080.

           EXEC SQL
             SELECT MAX(DFIM_ORD_AGPTO)
             INTO  :GFCTB080.DFIM-ORD-AGPTO
             FROM   DB2PRD.HIERQ_AGPTO_PPRIO
                    WHERE CSERVC_TARIF = :GFCTB080.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO '
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24321-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24322-OBTER-DATA-INICIO         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFI-COD-TARIFA(IND) TO CSERVC-TARIF OF GFCTB080.
           MOVE GFCTFI-DFIM-ORD-AGPTO(IND)
                                       TO DFIM-ORD-AGPTO OF GFCTB080.

           EXEC SQL
             SELECT MIN(DINIC_ORD_AGPTO)
             INTO  :GFCTB080.DINIC-ORD-AGPTO
             FROM   DB2PRD.HIERQ_AGPTO_PPRIO
             WHERE  DFIM_ORD_AGPTO =     :GFCTB080.DFIM-ORD-AGPTO
             AND    CSERVC_TARIF   =     :GFCTB080.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO '
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24322-99-FIM.                   EXIT.
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
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0008'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
                                       OR IND EQUAL 6
               PERFORM 25300-TRATAR-CAMPOS-CSR2
               PERFORM 25200-LER-CSR-2
           END-PERFORM.

           IF  IND                     EQUAL 6   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB080
                                       TO GFCTFI-PONTEIRO-TARIFA
           END-IF.

           PERFORM 25400-FECHAR-CSR-2.

      *----------------------------------------------------------------*
       25000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25100-ABRIR-CSR-2               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB080.
           MOVE WRK-DATA-REFER         TO DINIC-ORD-AGPTO   OF GFCTB080
                                          DFIM-ORD-AGPTO    OF GFCTB080.

           EXEC SQL
               OPEN CSR-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0009'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
               INTO    :GFCTB080.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFI-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE 0426           TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTFI-COD-SQL-ERRO
                   MOVE 'GFCTB080'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFI-DESC-MSG-ERRO
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
               MOVE 'V'                TO GFCTFI-TIPO-VIGENCIA(IND)
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
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0011'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
                                       OR IND EQUAL 6
               PERFORM 26300-TRATAR-CAMPOS-CSR3
               PERFORM 26200-LER-CSR-3
           END-PERFORM.

           IF  IND                     EQUAL 6   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB080
                                       TO GFCTFI-PONTEIRO-TARIFA
           END-IF.

           PERFORM 26400-FECHAR-CSR-3.

      *----------------------------------------------------------------*
       26000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26100-ABRIR-CSR-3               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB080.
           MOVE WRK-DATA-REFER         TO DINIC-ORD-AGPTO   OF GFCTB080
                                          DFIM-ORD-AGPTO    OF GFCTB080.

           EXEC SQL
               OPEN CSR-3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0012'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
               INTO    :GFCTB080.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0013'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFI-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE 0427           TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTFI-COD-SQL-ERRO
                   MOVE 'GFCTB080'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFI-DESC-MSG-ERRO
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
               MOVE ' '                TO GFCTFI-TIPO-VIGENCIA(IND)
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
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0014'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
                                       OR IND EQUAL 6
               PERFORM 27300-TRATAR-CAMPOS-CSR4
               PERFORM 27200-LER-CSR-4
           END-PERFORM.

           IF  IND                     EQUAL 6   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB080
                                       TO GFCTFI-PONTEIRO-TARIFA
               MOVE DINIC-ORD-AGPTO    OF GFCTB080
                                       TO GFCTFI-PONTEIRO-DATA
           END-IF.

           PERFORM 27400-FECHAR-CSR-4.

      *----------------------------------------------------------------*
       27000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27100-ABRIR-CSR-4               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF    OF GFCTB080.
           MOVE WRK-DATA-REFER         TO DINIC-ORD-AGPTO OF GFCTB080
                                          DFIM-ORD-AGPTO  OF GFCTB080.

           IF  GFCTFH-PONTEIRO-TARIFA  EQUAL ZEROS
               MOVE '01.01.0001'       TO WRK-PONTEIRO-DATA
           ELSE
               MOVE GFCTFH-PONTEIRO-DATA
                                       TO WRK-PONTEIRO-DATA
           END-IF.

           EXEC SQL
               OPEN CSR-4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0015'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
               INTO    :GFCTB080.CSERVC-TARIF,
                       :GFCTB080.DINIC-ORD-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0016'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFI-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE 0428           TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTFI-COD-SQL-ERRO
                   MOVE 'GFCTB080'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFI-DESC-MSG-ERRO
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

           STRING DINIC-ORD-AGPTO(07:04)
                  DINIC-ORD-AGPTO(04:02)
                  DINIC-ORD-AGPTO(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA1.

           STRING WRK-DATA-REFER(07:04)
                  WRK-DATA-REFER(04:02)
                  WRK-DATA-REFER(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA2.

           IF  WRK-DATA1               GREATER WRK-DATA2
               MOVE 'P'                TO GFCTFI-TIPO-VIGENCIA(IND)
           ELSE
               MOVE 'V'                TO GFCTFI-TIPO-VIGENCIA(IND)
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
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0017'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
                                       OR IND EQUAL 6
               PERFORM 28300-TRATAR-CAMPOS-CSR5
               PERFORM 28200-LER-CSR-5
           END-PERFORM.

           IF  IND                     EQUAL 6   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB080
                                       TO GFCTFI-PONTEIRO-TARIFA
           END-IF.

           PERFORM 28400-FECHAR-CSR-5.

      *----------------------------------------------------------------*
       28000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28100-ABRIR-CSR-5               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF    OF GFCTB080.
           MOVE WRK-DATA-REFER         TO DFIM-ORD-AGPTO  OF GFCTB080.

           EXEC SQL
               OPEN CSR-5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0018'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
               INTO    :GFCTB080.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0019'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFI-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE 0429           TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTFI-COD-SQL-ERRO
                   MOVE 'GFCTB080'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFI-DESC-MSG-ERRO
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
               MOVE 'H'                TO GFCTFI-TIPO-VIGENCIA(IND)
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
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
                                       OR IND EQUAL 6
               PERFORM 29300-TRATAR-CAMPOS-CSR6
               PERFORM 29200-LER-CSR-6
           END-PERFORM.

           IF  IND                     EQUAL 6   AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB080
                                       TO GFCTFI-PONTEIRO-TARIFA
           END-IF.

           PERFORM 29400-FECHAR-CSR-6.

      *----------------------------------------------------------------*
       29000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29100-ABRIR-CSR-6               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB080.
           MOVE WRK-DATA-REFER         TO DINIC-ORD-AGPTO   OF GFCTB080.

           EXEC SQL
               OPEN CSR-6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0021'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
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
               INTO    :GFCTB080.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0022'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFI-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTFI-ERRO
                   MOVE 0430           TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTFI-COD-SQL-ERRO
                   MOVE 'GFCTB080'     TO WRK-NOME-TAB
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFI-DESC-MSG-ERRO
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
               MOVE 'F'                TO GFCTFI-TIPO-VIGENCIA(IND)
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
               MOVE 'HIERQ_AGPTO_PPRIO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFI-COD-SQL-ERRO
               MOVE '0023'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTFI-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB080'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           IF  GFCTFI-ERRO             EQUAL 1
               MOVE 'S'                TO GFCTFI-FIM
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
