      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5073.
       AUTHOR      ROBSON FAUSTO CESA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5073                                    *
      *    PROGRAMADOR.:   ROBSON FAUSTO CESA        - CPM PATO BRANCO *
      *    ANALISTA CPM:   KHARUZO INOCENCIO LEITE   - CPM PATO BRANCO *
      *    ANALISTA....:   RICARDO PEREIRA           - PCW / GP 50     *
      *    DATA........:   12/07/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA COMBO DE TARIFAS DO TIPO   *
      *      DE OPERACAO.                                              *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                TABLE                        INCLUDE/BOOK       *
      *                    DB2PRD.TALCAD_LCTO_EXCEC   GFCTB0H0         *
      *                    DB2PRD.SERVC_TARIF_PRINC   GFCTB0D8         *
      *                    DB2PRD.PARM_DATA_PROCM     GFCTB0A1         *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTCW  -  AREA DE COMUNICACAO - ENTRADA                  *
      *    I#GFCTCZ  -  AREA DE COMUNICACAO - SAIDA                    *
      *    I#GFCT0M  -  AREA DE COMUNICACAO - ERROS                    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5000  -  OBTER DESCRICAO DE MENSAGENS                   *
      *    GFCT5500  -  VERIFICA ON-LINE                               *
      *    GFCT5507   - VERIFICA MASTER                                *
      *    POOL0081  -  ALOCAR DINAMICAMENTE UM MODULO                 *
      *    POOL1470  -  CONSISTE DATA COM SECULO                       *
      *    SENH0315  -  OBTER JUNCAO                                   *
      *================================================================*
           EJECT
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
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT5073 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)          VALUE
           '*** AREA PARA INDEXADORES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC 9(003)          VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)          VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA1                   PIC 9(008)          VALUE ZEROS.
       01  WRK-DATA2                   PIC 9(008)          VALUE ZEROS.
       01  WRK-FINALIZAR               PIC X(001)          VALUE SPACES.
       01  WRK-ERRO-DATA               PIC X(001)          VALUE SPACES.
       01  WRK-MODULO                  PIC X(008)          VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(008)          VALUE SPACES.
       01  WRK-PONTEIRO-DATA           PIC X(010)          VALUE SPACES.

       01  WRK-AUX-PONTEIRO.
           03  WRK-AUX-TARIFA          PIC 9(005)          VALUE ZEROS.

       01  WRK-TIPO-VIGENCIA           PIC X(001)          VALUE SPACES.

       01  WRK-DATA-DD-MM-AAAA.
           03  WRK-DATA-DD             PIC 9(002)          VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '.'.
           03  WRK-DATA-MM             PIC 9(002)          VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '.'.
           03  WRK-DATA-AAAA           PIC 9(004)          VALUE ZEROS.

       01  WRK-DATA-REFER.
           03  WRK-DTREF-DIA           PIC 9(002)          VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '.'.
           03  WRK-DTREF-MES           PIC 9(002)          VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '.'.
           03  WRK-DTREF-ANO           PIC 9(004)          VALUE ZEROS.

       01  WRK-CAD-DATA.
           03  WRK-CAD-DIA             PIC 9(002)          VALUE ZEROS.
           03  WRK-CAD-MES             PIC 9(002)          VALUE ZEROS.
           03  WRK-CAD-ANO             PIC 9(004)          VALUE ZEROS.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA PARA MODULO GFCT5500 ***'.
      *----------------------------------------------------------------*

       01  WRK-5500-AREA-SAIDA.
           03  WRK-5500-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           03  WRK-5500-COD-SQL-ERRO   PIC  9(003)         VALUE ZEROS.
           03  WRK-5500-DESC-MSG       PIC  X(070)         VALUE SPACES.

       01  WRK-IO-PCB                  PIC  X(032)         VALUE SPACES.
       01  WRK-ALT-PCB                 PIC  X(032)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA PARA MODULO GFCT5000 ***'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE       PIC X(001)      VALUE SPACES.
               05  WRK-5000-COD-MSG        PIC 9(004)      VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO    PIC 9(002)      VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO   PIC 9(003)      VALUE ZEROS.
               05  WRK-5000-DESC-MSG       PIC X(070)      VALUE SPACES.

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

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*** AREAS DE TABELAS DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB0H0
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB0D8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA PARA DECLARACAO DE CURSOR ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR-GFCTB0H0-1 CURSOR FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.TALCAD_LCTO_EXCEC
             WHERE
                   CTPO_OPER_MOTVO    = :GFCTB0H0.CTPO-OPER-MOTVO    AND
                   CSERVC_TARIF      >= :GFCTB0H0.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EJECT
           EXEC SQL
             DECLARE CSR-GFCTB0H0-2 CURSOR FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.TALCAD_LCTO_EXCEC
             WHERE
                   CTPO_OPER_MOTVO    = :GFCTB0H0.CTPO-OPER-MOTVO    AND
                   CSERVC_TARIF      >= :GFCTB0H0.CSERVC-TARIF       AND
                   DINIC_ALCAD_EXCEC <= :GFCTB0H0.DINIC-ALCAD-EXCEC  AND
                   DFIM_ALCAD_EXCEC  >= :GFCTB0H0.DFIM-ALCAD-EXCEC
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR-GFCTB0H0-3 CURSOR FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.TALCAD_LCTO_EXCEC
             WHERE
                  CTPO_OPER_MOTVO    = :GFCTB0H0.CTPO-OPER-MOTVO    AND
                  CSERVC_TARIF      >= :GFCTB0H0.CSERVC-TARIF       AND
                 (DINIC_ALCAD_EXCEC  < :GFCTB0H0.DINIC-ALCAD-EXCEC  OR
                  DFIM_ALCAD_EXCEC   > :GFCTB0H0.DFIM-ALCAD-EXCEC)
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR-GFCTB0H0-4 CURSOR FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.TALCAD_LCTO_EXCEC
            WHERE  CTPO_OPER_MOTVO    = :GFCTB0H0.CTPO-OPER-MOTVO    AND
                 ((CSERVC_TARIF      >= :GFCTB0H0.CSERVC-TARIF       AND
                   DINIC_ALCAD_EXCEC >= :WRK-PONTEIRO-DATA         ) OR
                   CSERVC_TARIF       > :GFCTB0H0.CSERVC-TARIF     ) AND
                 ((DINIC_ALCAD_EXCEC <= :GFCTB0H0.DINIC-ALCAD-EXCEC  AND
                   DFIM_ALCAD_EXCEC  >= :GFCTB0H0.DFIM-ALCAD-EXCEC)  OR
                  (DINIC_ALCAD_EXCEC >  :GFCTB0H0.DINIC-ALCAD-EXCEC))
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR-GFCTB0H0-5 CURSOR FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.TALCAD_LCTO_EXCEC
             WHERE
                   CTPO_OPER_MOTVO    = :GFCTB0H0.CTPO-OPER-MOTVO    AND
                   CSERVC_TARIF      >= :GFCTB0H0.CSERVC-TARIF       AND
                   DFIM_ALCAD_EXCEC   < :GFCTB0H0.DFIM-ALCAD-EXCEC
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR-GFCTB0H0-6 CURSOR FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.TALCAD_LCTO_EXCEC
             WHERE
                   CTPO_OPER_MOTVO    = :GFCTB0H0.CTPO-OPER-MOTVO    AND
                   CSERVC_TARIF      >= :GFCTB0H0.CSERVC-TARIF       AND
                   DINIC_ALCAD_EXCEC  > :GFCTB0H0.DINIC-ALCAD-EXCEC
             ORDER BY CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT5073 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC I#GFCTCW

           EJECT
-INC I#GFCTCZ

           EJECT
-INC I#GFCT0M

           EJECT
      *================================================================*
       PROCEDURE DIVISION              USING GFCTCW-ENTRADA
                                             GFCTCZ-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-PROCESSAR.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           PERFORM 110000-INICIALIZA-SAIDA.

           PERFORM 120000-CONSISTIR-DADOS.

           PERFORM 130000-VERIFICAR-ONLINE.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-INICIALIZA-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTCZ-SAIDA.

           INITIALIZE GFCTCZ-SAIDA.

           MOVE +266                   TO GFCTCZ-LL.
           MOVE GFCTCW-ZZ              TO GFCTCZ-ZZ.
           MOVE GFCTCW-TRANSACAO       TO GFCTCZ-TRANSACAO.
           MOVE GFCTCW-FUNCAO          TO GFCTCZ-FUNCAO.
           MOVE GFCTCW-CTPO-OPER-MOVTO TO GFCTCZ-CTPO-OPER-MOVTO.
           MOVE GFCTCW-FUNC-BDSCO      TO GFCTCZ-FUNC-BDSCO.
           MOVE GFCTCW-TERMINAL        TO GFCTCZ-TERMINAL.
           MOVE GFCTCW-PONTEIRO-TARIFA TO GFCTCZ-PONTEIRO-TARIFA.
           MOVE GFCTCW-PONTEIRO-DATA   TO GFCTCZ-PONTEIRO-DATA.
           MOVE ZEROS                  TO GFCTCZ-ERRO
                                          GFCTCZ-COD-SQL-ERRO
                                          GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.
           MOVE 'N'                    TO GFCTCZ-FIM.

           IF  GFCTCW-QTDE-OCOR        IS NUMERIC      AND
               GFCTCW-QTDE-OCOR        GREATER ZEROS
               MOVE GFCTCW-QTDE-OCOR   TO GFCTCZ-QTDE-OCOR
           ELSE
               MOVE ZEROS              TO GFCTCZ-QTDE-OCOR
           END-IF.

           IF  GFCTCW-QTDE-TOT-REG     IS NUMERIC      AND
               GFCTCW-QTDE-TOT-REG     GREATER ZEROS
               MOVE GFCTCW-QTDE-TOT-REG
                                       TO GFCTCZ-QTDE-TOT-REG
           ELSE
               MOVE ZEROS              TO GFCTCZ-QTDE-TOT-REG
           END-IF.

           PERFORM 111000-OBTER-DESC-MSG.

           MOVE WRK-5000-DESC-MSG      TO GFCTCZ-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-OBTER-DESC-MSG           SECTION.
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
                   MOVE 1              TO GFCTCZ-ERRO
                   MOVE ZEROS          TO GFCTCZ-COD-SQL-ERRO
                                          GFCTCZ-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCZ-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTCZ-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTCZ-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTCZ-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCZ-DESC-MSG-ERRO
               END-IF
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           IF (GFCTCW-TRANSACAO        EQUAL SPACES)           OR
              (GFCTCW-FUNCAO           EQUAL SPACES)           OR
              (GFCTCW-CTPO-OPER-MOVTO  NOT NUMERIC)            OR
              (GFCTCW-QTDE-OCOR        NOT NUMERIC)            OR
              (GFCTCW-QTDE-TOT-REG     NOT NUMERIC)            OR
              (GFCTCW-FUNC-BDSCO       EQUAL SPACES)           OR
              (GFCTCW-PONTEIRO-TARIFA  NOT NUMERIC)            OR
              (GFCTCW-FIM              NOT EQUAL 'S' AND 'N')  OR
              (GFCTCW-TIPO-VIGENCIA    NOT EQUAL 'V' AND 'N'   AND
                                                 'T' AND 'P'   AND
                                                 'H' AND 'F')  OR
              (GFCTCW-TIPO-DATA        NOT EQUAL 0 AND 1 AND
                                                 2 AND 3    )  OR
              (GFCTCW-TIPO-DATA        EQUAL 0 AND
               GFCTCW-DATA-REFER       EQUAL SPACES)
               MOVE 1                  TO GFCTCZ-ERRO
               MOVE ZEROS              TO GFCTCZ-COD-SQL-ERRO
               MOVE 0001               TO GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCZ-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       130000-VERIFICAR-ONLINE         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5500-AREA-SAIDA.

           MOVE 'GFCT5500'                 TO WRK-MODULO.

           CALL 'POOL0081'                 USING WRK-MODULO
                                                 WRK-5500-AREA-SAIDA
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB.

           IF  WRK-5500-COD-RETORNO        NOT EQUAL ZEROS
               IF  WRK-5500-COD-RETORNO
                                           EQUAL 04
                   MOVE 1                  TO GFCTCZ-ERRO
                   MOVE ZEROS              TO GFCTCZ-COD-SQL-ERRO
                   MOVE 030                TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG  TO GFCTCZ-DESC-MSG-ERRO
               END-IF
               IF  WRK-5500-COD-RETORNO    EQUAL 99
                   INITIALIZE GFCT0M-AREA-ERROS
                   MOVE 'GFCT5073'         TO GFCT0M-PROGRAMA
                   MOVE 'PARM_CTRL_GSTAO'  TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                           TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
                   MOVE '0010'             TO GFCT0M-LOCAL
                   MOVE 12                 TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   MOVE 'GFCTB0A0'         TO WRK-NOME-TAB
                   PERFORM 131000-MOVER-ERRO-SQL
               END-IF
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       130000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       131000-MOVER-ERRO-SQL           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5073'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO GFCTCZ-ERRO.

           PERFORM 111000-OBTER-DESC-MSG.

           PERFORM 131100-CONCATENAR-MSG.

      *----------------------------------------------------------------*
       131000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       131100-CONCATENAR-MSG           SECTION.
      *----------------------------------------------------------------*

           STRING WRK-5000-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO GFCTCZ-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       131100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 201000-OBTER-COD-JUNCAO.

           PERFORM 202000-VERIFICAR-MASTER.

           IF GFCTCW-PONTEIRO-DATA     EQUAL  SPACES
              MOVE '01.01.0001'        TO GFCTCW-PONTEIRO-DATA
           END-IF.

           PERFORM 210000-FORMATAR-DT-REF.

           MOVE GFCTCW-PONTEIRO-TARIFA TO WRK-AUX-TARIFA.
           MOVE ZEROS                  TO IND-1.
           MOVE SPACES                 TO WRK-FINALIZAR.

           EVALUATE GFCTCW-TIPO-VIGENCIA
               WHEN 'T'
                   PERFORM 220000-TRATAR-CSR-GFCTB0H0-1
               WHEN 'V'
                   PERFORM 230000-TRATAR-CSR-GFCTB0H0-2
               WHEN 'N'
                   PERFORM 240000-TRATAR-CSR-GFCTB0H0-3
               WHEN 'P'
                   PERFORM 250000-TRATAR-CSR-GFCTB0H0-4
               WHEN 'H'
                   PERFORM 260000-TRATAR-CSR-GFCTB0H0-5
               WHEN OTHER
                   PERFORM 270000-TRATAR-CSR-GFCTB0H0-6
           END-EVALUATE.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       201000-OBTER-COD-JUNCAO         SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCTCW-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                  TO GFCTCZ-ERRO
               MOVE RETURN-CODE        TO GFCTCZ-COD-SQL-ERRO
               MOVE 56                 TO GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCZ-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               EVALUATE WRK-COD-MENSAGEM-0315
                   WHEN 1
                       MOVE 1          TO GFCTCZ-ERRO
                       MOVE 57         TO GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 154
                       MOVE 1          TO GFCTCZ-ERRO
                       MOVE 58         TO GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 242
                       MOVE 1          TO GFCTCZ-ERRO
                       MOVE 59         TO GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 9
                       MOVE ZEROS      TO GFCTCZ-ERRO
                                          GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                       MOVE 'S'        TO GFCTCZ-FIM
                   WHEN 138
                       MOVE 1          TO GFCTCZ-ERRO
                       MOVE 60         TO GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               END-EVALUATE
               MOVE ZEROS              TO GFCTCZ-COD-SQL-ERRO
               PERFORM 111000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCZ-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       201000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       202000-VERIFICAR-MASTER         SECTION.
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
                   MOVE 1              TO GFCTCZ-ERRO
                   MOVE ZEROS          TO GFCTCZ-COD-SQL-ERRO
                                          GFCTCZ-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTCZ-DESC-MSG-ERRO
               END-IF
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTCZ-ERRO
                   MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCTCZ-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTCZ-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTCZ-DESC-MSG-ERRO
               END-IF
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       202000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       210000-FORMATAR-DT-REF          SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCW-TIPO-DATA        EQUAL ZEROS
               MOVE GFCTCW-DATA-REFER  TO WRK-DATA-DD-MM-AAAA
               PERFORM 211000-CONSISTIR-DT-WEB
               IF  WRK-ERRO-DATA       EQUAL 'S'
                   MOVE 1              TO GFCTCZ-ERRO
                   MOVE ZEROS          TO GFCTCZ-COD-SQL-ERRO
                   MOVE 125            TO GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCZ-DESC-MSG-ERRO
                   PERFORM 300000-FINALIZAR
               ELSE
                   MOVE WRK-DATA-AAAA  TO WRK-DTREF-ANO
                   MOVE WRK-DATA-MM    TO WRK-DTREF-MES
                   MOVE WRK-DATA-DD    TO WRK-DTREF-DIA
               END-IF
           ELSE
               PERFORM 212000-OBTER-DT-PROCE
               IF  GFCTCW-TIPO-DATA    EQUAL 1
                   MOVE DPROCM-ANTER OF GFCTB0A1
                                       TO WRK-DATA-REFER
               ELSE
                   IF  GFCTCW-TIPO-DATA
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
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211000-CONSISTIR-DT-WEB         SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ERRO-DATA.

           IF  WRK-DATA-DD-MM-AAAA     EQUAL SPACES OR LOW-VALUES
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
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       212000-OBTER-DT-PROCE           SECTION.
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
                                          GFCTCZ-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0010               TO GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       212000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       220000-TRATAR-CSR-GFCTB0H0-1    SECTION.
      *----------------------------------------------------------------*

           PERFORM 221000-ABRIR-CSR-GFCTB0H0-1.

           PERFORM 222000-LER-CSR-GFCTB0H0-1.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND-1 GREATER 7
               MOVE 'T'                TO WRK-TIPO-VIGENCIA

               PERFORM 223000-PROCES-CSR-GFCTB0H0-1
           END-PERFORM.

           IF  IND-1                   GREATER 7
               MOVE CSERVC-TARIF       OF GFCTB0H0
                                       TO WRK-AUX-TARIFA
               MOVE WRK-AUX-PONTEIRO   TO GFCTCZ-PONTEIRO
           END-IF.

           PERFORM 224000-FECHAR-CSR-GFCTB0H0-1.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       221000-ABRIR-CSR-GFCTB0H0-1     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF    OF GFCTB0H0.
           MOVE GFCTCW-CTPO-OPER-MOVTO TO CTPO-OPER-MOTVO OF GFCTB0H0.

           EXEC SQL
               OPEN CSR-GFCTB0H0-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCZ-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'         TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       221000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       222000-LER-CSR-GFCTB0H0-1       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR-GFCTB0H0-1 INTO
                   :GFCTB0H0.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0040'                 TO GFCT0M-LOCAL
               MOVE 0006                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 'S'                    TO WRK-FINALIZAR
                                              GFCTCZ-FIM
               IF  IND-1                   EQUAL ZEROS
                   MOVE 1                  TO GFCTCZ-ERRO
                   MOVE 0313               TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   MOVE ZEROS              TO GFCTCZ-COD-SQL-ERRO
                   MOVE 'GFCTB0H0'         TO WRK-NOME-TAB
                   PERFORM 111000-OBTER-DESC-MSG
                   PERFORM 131100-CONCATENAR-MSG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       222000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       223000-PROCES-CSR-GFCTB0H0-1    SECTION.
      *----------------------------------------------------------------*

           PERFORM 290000-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  223000-99-FIM
               END-IF

               PERFORM 280000-TRATAR-CURSOR
           END-IF.

           PERFORM 222000-LER-CSR-GFCTB0H0-1.

      *----------------------------------------------------------------*
       223000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       224000-FECHAR-CSR-GFCTB0H0-1    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-GFCTB0H0-1
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0060'                 TO GFCT0M-LOCAL
               MOVE 0011                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       224000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       230000-TRATAR-CSR-GFCTB0H0-2    SECTION.
      *----------------------------------------------------------------*

           PERFORM 231000-ABRIR-CSR-GFCTB0H0-2.

           PERFORM 232000-LER-CSR-GFCTB0H0-2.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND-1 GREATER 7
               MOVE 'V'                TO WRK-TIPO-VIGENCIA

               PERFORM 233000-PROCES-CSR-GFCTB0H0-2
           END-PERFORM.

           IF  IND-1                   GREATER 7
               MOVE CSERVC-TARIF       OF GFCTB0H0
                                       TO WRK-AUX-TARIFA
               MOVE WRK-AUX-PONTEIRO   TO GFCTCZ-PONTEIRO
           END-IF.

           PERFORM 234000-FECHAR-CSR-GFCTB0H0-2.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       231000-ABRIR-CSR-GFCTB0H0-2     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA             TO CSERVC-TARIF
                                           OF GFCTB0H0.
           MOVE GFCTCW-CTPO-OPER-MOVTO     TO CTPO-OPER-MOTVO
                                           OF GFCTB0H0.
           MOVE WRK-DATA-REFER             TO DINIC-ALCAD-EXCEC
                                           OF GFCTB0H0
                                              DFIM-ALCAD-EXCEC
                                           OF GFCTB0H0.

           EXEC SQL
               OPEN CSR-GFCTB0H0-2
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'                 TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0070'                 TO GFCT0M-LOCAL
               MOVE 0005                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       231000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       232000-LER-CSR-GFCTB0H0-2       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-GFCTB0H0-2
               INTO    :GFCTB0H0.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0080'                 TO GFCT0M-LOCAL
               MOVE 0006                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 'S'                    TO WRK-FINALIZAR
                                              GFCTCZ-FIM
               IF  IND-1                   EQUAL ZEROS
                   MOVE 1                  TO GFCTCZ-ERRO
                   MOVE 0314               TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   MOVE ZEROS              TO GFCTCZ-COD-SQL-ERRO
                   MOVE 'GFCTB0H0'         TO WRK-NOME-TAB
                   PERFORM 111000-OBTER-DESC-MSG
                   PERFORM 131100-CONCATENAR-MSG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       232000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       233000-PROCES-CSR-GFCTB0H0-2   SECTION.
      *----------------------------------------------------------------*

           PERFORM 290000-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  233000-99-FIM
               END-IF

               PERFORM 280000-TRATAR-CURSOR
           END-IF.

           PERFORM 232000-LER-CSR-GFCTB0H0-2.

      *----------------------------------------------------------------*
       233000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234000-FECHAR-CSR-GFCTB0H0-2    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-GFCTB0H0-2
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0090'                 TO GFCT0M-LOCAL
               MOVE 0011                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       234000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       240000-TRATAR-CSR-GFCTB0H0-3    SECTION.
      *----------------------------------------------------------------*

           PERFORM 241000-ABRIR-CSR-GFCTB0H0-3.

           PERFORM 242000-LER-CSR-GFCTB0H0-3.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND-1 GREATER 7
               MOVE SPACES             TO WRK-TIPO-VIGENCIA

               PERFORM 243000-PROCES-CSR-GFCTB0H0-3
           END-PERFORM.

           IF  IND-1                   GREATER 7
               MOVE CSERVC-TARIF       OF GFCTB0H0
                                       TO WRK-AUX-TARIFA
               MOVE WRK-AUX-PONTEIRO   TO GFCTCZ-PONTEIRO
           END-IF.

           PERFORM 244000-FECHAR-CSR-GFCTB0H0-3.

      *----------------------------------------------------------------*
       240000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       241000-ABRIR-CSR-GFCTB0H0-3     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA             TO CSERVC-TARIF
                                           OF GFCTB0H0.
           MOVE GFCTCW-CTPO-OPER-MOVTO     TO CTPO-OPER-MOTVO
                                           OF GFCTB0H0.
           MOVE WRK-DATA-REFER             TO DINIC-ALCAD-EXCEC
                                           OF GFCTB0H0
                                              DFIM-ALCAD-EXCEC
                                           OF GFCTB0H0.

           EXEC SQL
               OPEN CSR-GFCTB0H0-3
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'                 TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0100'                 TO GFCT0M-LOCAL
               MOVE 0005                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       241000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       242000-LER-CSR-GFCTB0H0-3       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-GFCTB0H0-3
               INTO    :GFCTB0H0.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0110'                 TO GFCT0M-LOCAL
               MOVE 0006                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 'S'                    TO WRK-FINALIZAR
                                              GFCTCZ-FIM
               IF  IND-1                   EQUAL ZEROS
                   MOVE 1                  TO GFCTCZ-ERRO
                   MOVE 0315               TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   MOVE ZEROS              TO GFCTCZ-COD-SQL-ERRO
                   MOVE 'GFCTB0H0'         TO WRK-NOME-TAB
                   PERFORM 111000-OBTER-DESC-MSG
                   PERFORM 131100-CONCATENAR-MSG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       242000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       243000-PROCES-CSR-GFCTB0H0-3    SECTION.
      *----------------------------------------------------------------*

           PERFORM 290000-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  243000-99-FIM
               END-IF

               PERFORM 280000-TRATAR-CURSOR
           END-IF.

           PERFORM 242000-LER-CSR-GFCTB0H0-3.

      *----------------------------------------------------------------*
       243000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       244000-FECHAR-CSR-GFCTB0H0-3    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-GFCTB0H0-3
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0120'                 TO GFCT0M-LOCAL
               MOVE 0011                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       244000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       250000-TRATAR-CSR-GFCTB0H0-4    SECTION.
      *----------------------------------------------------------------*

           PERFORM 251000-ABRIR-CSR-GFCTB0H0-4.

           PERFORM 252000-LER-CSR-GFCTB0H0-4.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND-1 GREATER 7
               PERFORM 253100-COMPARAR-DATAS

               PERFORM 253000-PROCES-CSR-GFCTB0H0-4
           END-PERFORM.

           IF  IND-1                   GREATER 7
               MOVE CSERVC-TARIF       OF GFCTB0H0
                                       TO WRK-AUX-TARIFA
               MOVE WRK-AUX-PONTEIRO   TO GFCTCZ-PONTEIRO
           END-IF.

           PERFORM 254000-FECHAR-CSR-GFCTB0H0-4.

      *----------------------------------------------------------------*
       250000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       251000-ABRIR-CSR-GFCTB0H0-4     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA             TO CSERVC-TARIF
                                           OF GFCTB0H0.
           MOVE GFCTCW-CTPO-OPER-MOVTO     TO CTPO-OPER-MOTVO
                                           OF GFCTB0H0.
           MOVE WRK-DATA-REFER             TO DINIC-ALCAD-EXCEC
                                           OF GFCTB0H0
                                              DFIM-ALCAD-EXCEC
                                           OF GFCTB0H0.
           MOVE GFCTCW-PONTEIRO-DATA       TO WRK-PONTEIRO-DATA.

           EXEC SQL
               OPEN CSR-GFCTB0H0-4
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'                 TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0130'                 TO GFCT0M-LOCAL
               MOVE 0005                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       251000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       252000-LER-CSR-GFCTB0H0-4       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-GFCTB0H0-4
               INTO    :GFCTB0H0.CSERVC-TARIF,
                       :GFCTB0H0.DINIC-ALCAD-EXCEC
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0140'                 TO GFCT0M-LOCAL
               MOVE 0006                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 'S'                    TO WRK-FINALIZAR
                                              GFCTCZ-FIM
               IF  IND-1                   EQUAL ZEROS
                   MOVE 1                  TO GFCTCZ-ERRO
                   MOVE 0316               TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   MOVE ZEROS              TO GFCTCZ-COD-SQL-ERRO
                   MOVE 'GFCTB0H0'         TO WRK-NOME-TAB
                   PERFORM 111000-OBTER-DESC-MSG
                   PERFORM 131100-CONCATENAR-MSG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       252000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       253000-PROCES-CSR-GFCTB0H0-4   SECTION.
      *----------------------------------------------------------------*

           PERFORM 290000-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  253000-99-FIM
               END-IF

               PERFORM 280000-TRATAR-CURSOR
           END-IF.

           PERFORM 252000-LER-CSR-GFCTB0H0-4.

      *----------------------------------------------------------------*
       253000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       253100-COMPARAR-DATAS           SECTION.
      *----------------------------------------------------------------*

           STRING DINIC-ALCAD-EXCEC(07:04)
                  DINIC-ALCAD-EXCEC(04:02)
                  DINIC-ALCAD-EXCEC(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA1.

           STRING WRK-DATA-REFER(07:04)
                  WRK-DATA-REFER(04:02)
                  WRK-DATA-REFER(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA2.

           IF  WRK-DATA1               GREATER WRK-DATA2
               MOVE 'P'                TO WRK-TIPO-VIGENCIA
           ELSE
               MOVE 'V'                TO WRK-TIPO-VIGENCIA
           END-IF.

      *----------------------------------------------------------------*
       253100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       254000-FECHAR-CSR-GFCTB0H0-4    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-GFCTB0H0-4
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0150'                 TO GFCT0M-LOCAL
               MOVE 0011                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       254000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       260000-TRATAR-CSR-GFCTB0H0-5    SECTION.
      *----------------------------------------------------------------*

           PERFORM 261000-ABRIR-CSR-GFCTB0H0-5.

           PERFORM 262000-LER-CSR-GFCTB0H0-5.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND-1 GREATER 7
               MOVE 'H'                TO WRK-TIPO-VIGENCIA

               PERFORM 263000-PROCES-CSR-GFCTB0H0-5
           END-PERFORM.

           IF  IND-1                   GREATER 7
               MOVE CSERVC-TARIF       OF GFCTB0H0
                                       TO WRK-AUX-TARIFA
               MOVE WRK-AUX-PONTEIRO   TO GFCTCZ-PONTEIRO
           END-IF.

           PERFORM 264000-FECHAR-CSR-GFCTB0H0-5.

      *----------------------------------------------------------------*
       260000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       261000-ABRIR-CSR-GFCTB0H0-5     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA             TO CSERVC-TARIF
                                           OF GFCTB0H0.
           MOVE GFCTCW-CTPO-OPER-MOVTO     TO CTPO-OPER-MOTVO
                                           OF GFCTB0H0.
           MOVE WRK-DATA-REFER             TO DFIM-ALCAD-EXCEC
                                           OF GFCTB0H0.

           EXEC SQL
               OPEN CSR-GFCTB0H0-5
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'                 TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0160'                 TO GFCT0M-LOCAL
               MOVE 0005                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       261000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       262000-LER-CSR-GFCTB0H0-5       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-GFCTB0H0-5
               INTO    :GFCTB0H0.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0170'                 TO GFCT0M-LOCAL
               MOVE 0006                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 'S'                    TO WRK-FINALIZAR
                                              GFCTCZ-FIM
               IF  IND-1                   EQUAL ZEROS
                   MOVE 1                  TO GFCTCZ-ERRO
                   MOVE 0317               TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   MOVE ZEROS              TO GFCTCZ-COD-SQL-ERRO
                   MOVE 'GFCTB0H0'         TO WRK-NOME-TAB
                   PERFORM 111000-OBTER-DESC-MSG
                   PERFORM 131100-CONCATENAR-MSG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       262000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       263000-PROCES-CSR-GFCTB0H0-5   SECTION.
      *----------------------------------------------------------------*

           PERFORM 290000-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  263000-99-FIM
               END-IF

               PERFORM 280000-TRATAR-CURSOR
           END-IF.

           PERFORM 262000-LER-CSR-GFCTB0H0-5.

      *----------------------------------------------------------------*
       263000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       264000-FECHAR-CSR-GFCTB0H0-5    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-GFCTB0H0-5
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0180'                 TO GFCT0M-LOCAL
               MOVE 0011                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       264000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       270000-TRATAR-CSR-GFCTB0H0-6    SECTION.
      *----------------------------------------------------------------*

           PERFORM 271000-ABRIR-CSR-GFCTB0H0-6.

           PERFORM 272000-LER-CSR-GFCTB0H0-6.

           PERFORM                     UNTIL WRK-FINALIZAR EQUAL 'S'
                                       OR IND-1 GREATER 7
               MOVE 'F'                TO WRK-TIPO-VIGENCIA

               PERFORM 273000-PROCES-CSR-GFCTB0H0-6
           END-PERFORM.

           IF  IND-1                   GREATER 7
               MOVE CSERVC-TARIF       OF GFCTB0H0
                                       TO WRK-AUX-TARIFA
               MOVE WRK-AUX-PONTEIRO   TO GFCTCZ-PONTEIRO
           END-IF.

           PERFORM 274000-FECHAR-CSR-GFCTB0H0-6.

      *----------------------------------------------------------------*
       270000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       271000-ABRIR-CSR-GFCTB0H0-6     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA             TO CSERVC-TARIF
                                           OF GFCTB0H0.
           MOVE GFCTCW-CTPO-OPER-MOVTO     TO CTPO-OPER-MOTVO
                                           OF GFCTB0H0.
           MOVE WRK-DATA-REFER             TO DINIC-ALCAD-EXCEC
                                           OF GFCTB0H0.

           EXEC SQL
               OPEN CSR-GFCTB0H0-6
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'                 TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0190'                 TO GFCT0M-LOCAL
               MOVE 0005                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       271000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       272000-LER-CSR-GFCTB0H0-6       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-GFCTB0H0-6
               INTO    :GFCTB0H0.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0200'                 TO GFCT0M-LOCAL
               MOVE 0006                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 'S'                    TO WRK-FINALIZAR
                                              GFCTCZ-FIM
               IF  IND-1                   EQUAL ZEROS
                   MOVE 1                  TO GFCTCZ-ERRO
                   MOVE 0318               TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   MOVE ZEROS              TO GFCTCZ-COD-SQL-ERRO
                   MOVE 'GFCTB0H0'         TO WRK-NOME-TAB
                   PERFORM 111000-OBTER-DESC-MSG
                   PERFORM 131100-CONCATENAR-MSG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       272000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       273000-PROCES-CSR-GFCTB0H0-6   SECTION.
      *----------------------------------------------------------------*

           PERFORM 290000-ACESSAR-GFCTB0D8.

           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
               ADD 1                   TO IND-1

               IF  IND-1               GREATER 7
                   GO   TO  273000-99-FIM
               END-IF

               PERFORM 280000-TRATAR-CURSOR
           END-IF.

           PERFORM 272000-LER-CSR-GFCTB0H0-6.

      *----------------------------------------------------------------*
       273000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       274000-FECHAR-CSR-GFCTB0H0-6    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-GFCTB0H0-6
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TALCAD_LCTO_EXCEC'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
                                              GFCTCZ-COD-SQL-ERRO
               MOVE '0210'                 TO GFCT0M-LOCAL
               MOVE 0011                   TO GFCTCZ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'GFCTB0H0'             TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       274000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       280000-TRATAR-CURSOR            SECTION.
      *----------------------------------------------------------------*

           ADD 106                     TO GFCTCZ-LL.
           ADD 1                       TO GFCTCZ-QTDE-TOT-REG.

           MOVE CSERVC-TARIF           OF GFCTB0H0
                                       TO GFCTCZ-COD-TARIFA(IND-1).

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTCZ-DESC-TARIFA(IND-1).

           MOVE WRK-TIPO-VIGENCIA      TO GFCTCZ-TIPO-VIGENCIA(IND-1).

      *----------------------------------------------------------------*
       280000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       290000-ACESSAR-GFCTB0D8         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0H0
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
                                          GFCTCZ-COD-SQL-ERRO
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 0010               TO GFCTCZ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 131000-MOVER-ERRO-SQL
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       290000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCZ-ERRO             EQUAL 1
               MOVE 'S'                TO GFCTCZ-FIM
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
