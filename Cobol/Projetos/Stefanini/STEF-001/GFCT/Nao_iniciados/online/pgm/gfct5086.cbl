      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5086.
       AUTHOR. VIVIANE CARDOSO.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5086                                     *
      *    PROGRAMADOR  : VIVIANE CARDOSO               - CPM/FPOLIS   *
      *    ANALISTA CPM : SILVANA F. S. MACHADO         - CPM/FPOLIS   *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK    - GRUPO 50     *
      *    DATA         : 08/09/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA COMBO DE TARIFA.                         *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.PARM_SERVC_TARIF                GFCTB0D8          *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0A2          *
      *       DB2PRD.PARM_DATA_PROCM                 GFCTB0A1          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5500 - VERIFICA SE O SISTEMA ESTA LIBERADO            *
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
           '*  INICIO DA WORKING GFCT5086  *'.
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
       01  WRK-DATA                    PIC X(10)           VALUE SPACES.

       01  WRK-AUX-PONTEIRO.
           03  WRK-AUX-TARIFA          PIC 9(05)           VALUE ZEROS.
           03  WRK-AUX-DATA            PIC X(10)           VALUE SPACES.

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
       01  FILLER                      PIC  X(32)          VALUE
            '*    AREA DO MODULO GFCT5500   *'.
      *----------------------------------------------------------------*

       01  WRK-5500-AREA-SAIDA.
           03  WRK-5500-COD-RETORNO    PIC  9(02)          VALUE ZEROS.
           03  WRK-5500-COD-SQL-ERRO   PIC  9(03)          VALUE ZEROS.
           03  WRK-5500-DESC-MSG       PIC  X(70)          VALUE SPACES.

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
                INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             DECLARE CSR-1 CURSOR FOR
             SELECT  CSERVC_TARIF,
                     DINIC_VGCIA_TARIF,
                     DFIM_VGCIA_TARIF
             FROM    DB2PRD.PARM_SERVC_TARIF
             WHERE ((CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF
               AND   DINIC_VGCIA_TARIF >= :GFCTB0A2.DINIC-VGCIA-TARIF)
                OR   CSERVC_TARIF      >  :GFCTB0A2.CSERVC-TARIF)
               AND ((DINIC_VGCIA_TARIF <= :WRK-DATA
               AND   DFIM_VGCIA_TARIF  >= :WRK-DATA)
                OR  (DINIC_VGCIA_TARIF >  :WRK-DATA))
             ORDER   BY CSERVC_TARIF,
                        DINIC_VGCIA_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5086   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC I#GFCTFJ

-INC I#GFCTFK

-INC I#GFCT0M

      *================================================================*
       PROCEDURE DIVISION              USING GFCTFJ-ENTRADA
                                             GFCTFK-SAIDA
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

           MOVE SPACES                 TO GFCTFK-SAIDA.

           INITIALIZE GFCTFK-SAIDA.

           MOVE 260                    TO GFCTFK-LL.
           MOVE GFCTFJ-ZZ              TO GFCTFK-ZZ.
           MOVE GFCTFJ-TRANSACAO       TO GFCTFK-TRANSACAO.
           MOVE GFCTFJ-FUNCAO          TO GFCTFK-FUNCAO.
           MOVE GFCTFJ-FUNC-BDSCO      TO GFCTFK-FUNC-BDSCO.
           MOVE GFCTFJ-TERMINAL        TO GFCTFK-TERMINAL.
           MOVE GFCTFJ-PONTEIRO-TARIFA TO GFCTFK-PONTEIRO-TARIFA.
           MOVE GFCTFJ-PONTEIRO-DATA   TO GFCTFK-PONTEIRO-DATA.
           MOVE 'N'                    TO GFCTFK-FIM.
           MOVE ZEROS                  TO GFCTFK-ERRO
                                          GFCTFK-COD-SQL-ERRO
                                          GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           IF  GFCTFJ-QTDE-OCOR        IS NUMERIC
               MOVE GFCTFJ-QTDE-OCOR   TO GFCTFK-QTDE-OCOR
           END-IF.

           IF  GFCTFJ-QTDE-TOT-REG     IS NUMERIC
               MOVE GFCTFJ-QTDE-TOT-REG
                                       TO GFCTFK-QTDE-TOT-REG
           END-IF.

           PERFORM 11100-OBTER-DESC-MSG.

           MOVE WRK-5000-DESC-MSG      TO GFCTFK-DESC-MSG-ERRO.

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
                   MOVE 1              TO GFCTFK-ERRO
                   MOVE 'S'            TO GFCTFK-FIM
                   MOVE ZEROS          TO GFCTFK-COD-SQL-ERRO
                                          GFCTFK-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFK-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTFK-ERRO
                   MOVE 'S'            TO GFCTFK-FIM
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTFK-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTFK-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFK-DESC-MSG-ERRO
               END-IF
               INITIALIZE GFCTFK-DADOS
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           PERFORM 12100-VERIFICAR-ONLINE.

           IF (GFCTFJ-TRANSACAO        EQUAL SPACES)          OR
              (GFCTFJ-FUNCAO           EQUAL SPACES)          OR
              (GFCTFJ-QTDE-OCOR        IS NOT NUMERIC)        OR
              (GFCTFJ-QTDE-TOT-REG     IS NOT NUMERIC)        OR
              (GFCTFJ-FUNC-BDSCO       EQUAL SPACES)          OR
              (GFCTFJ-PONTEIRO-TARIFA  IS NOT NUMERIC)        OR
              (GFCTFJ-TIPO-DATA        EQUAL 0                AND
               GFCTFJ-PONTEIRO-DATA    EQUAL SPACES)          OR
              (GFCTFJ-FIM              NOT EQUAL 'S' AND 'N') OR
              (GFCTFJ-TIPO-DATA        IS NOT NUMERIC         OR
               GFCTFJ-TIPO-DATA        NOT EQUAL  0  AND  1   AND
                                                  2  AND  3 )
               MOVE 1                  TO GFCTFK-ERRO
               MOVE ZEROS              TO GFCTFK-COD-SQL-ERRO
               MOVE 0001               TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTFK-FIM
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTFK-DESC-MSG-ERRO
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
                   MOVE 1              TO GFCTFK-ERRO
                   MOVE 'S'            TO GFCTFK-FIM
                   MOVE ZEROS          TO GFCTFK-COD-SQL-ERRO
                   MOVE 030            TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFK-DESC-MSG-ERRO
               END-IF
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   INITIALIZE          GFCT0M-AREA-ERROS
                   MOVE 'PARM_CTRL_GSTAO'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTFK-COD-SQL-ERRO
                   MOVE '0001'         TO GFCT0M-LOCAL
                   MOVE 12             TO GFCTFK-COD-MSG-ERRO
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

           MOVE 'GFCT5086'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO GFCTFK-ERRO.

           PERFORM 11100-OBTER-DESC-MSG.

           MOVE SPACES                 TO GFCTFK-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO GFCTFK-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       12110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-OBTER-COD-JUNCAO.

           PERFORM 22000-VERIFICAR-MASTER.

           PERFORM 23000-FORMATAR-DT-REF.

           MOVE GFCTFJ-PONTEIRO-TARIFA TO WRK-AUX-TARIFA.

           IF  GFCTFJ-PONTEIRO-TARIFA  EQUAL ZEROS
               MOVE '01.01.0001'       TO WRK-AUX-DATA
           ELSE
               MOVE GFCTFJ-PONTEIRO-DATA
                                       TO WRK-AUX-DATA
           END-IF.

           PERFORM 24000-TRATAR-CSR-1.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCTFJ-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                  TO GFCTFK-ERRO
               MOVE RETURN-CODE        TO GFCTFK-COD-SQL-ERRO
               MOVE 56                 TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTFK-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               EVALUATE WRK-COD-MENSAGEM-0315
                   WHEN 1
                       MOVE 1          TO GFCTFK-ERRO
                       MOVE 57         TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 154
                       MOVE 1          TO GFCTFK-ERRO
                       MOVE 58         TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 242
                       MOVE 1          TO GFCTFK-ERRO
                       MOVE 59         TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   WHEN 9
                       MOVE ZEROS      TO GFCTFK-ERRO
                                          GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                       MOVE 'S'        TO GFCTFK-FIM
                   WHEN 138
                       MOVE 1          TO GFCTFK-ERRO
                       MOVE 60         TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               END-EVALUATE
               MOVE ZEROS              TO GFCTFK-COD-SQL-ERRO
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTFK-DESC-MSG-ERRO
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
                   MOVE 1              TO GFCTFK-ERRO
                   MOVE ZEROS          TO GFCTFK-COD-SQL-ERRO
                                          GFCTFK-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTFK-DESC-MSG-ERRO
               END-IF
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTFK-ERRO
                   MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCTFK-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTFK-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTFK-DESC-MSG-ERRO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-FORMATAR-DT-REF           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTFJ-TIPO-DATA        EQUAL ZEROS
               MOVE GFCTFJ-DATA-REFER  TO WRK-DATA-REFER
               PERFORM 23100-FORMATAR-DT-WEB
               IF  WRK-ERRO-DATA       EQUAL 'S'
                   MOVE 1              TO GFCTFK-ERRO
                   MOVE ZEROS          TO GFCTFK-COD-SQL-ERRO
                   MOVE 125            TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'S'            TO GFCTFK-FIM
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFK-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
           ELSE
               PERFORM 23200-OBTER-DT-PROCE
               IF  GFCTFJ-TIPO-DATA    EQUAL 1
                   MOVE DPROCM-ANTER OF GFCTB0A1
                                       TO WRK-DATA-REFER
               ELSE
                   IF  GFCTFJ-TIPO-DATA
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

           IF  WRK-DATA-REFER          EQUAL SPACES
               MOVE 'S'                TO WRK-ERRO-DATA
           ELSE
               MOVE WRK-DTREF-DIA      TO WRK-CAD-DIA
               MOVE WRK-DTREF-MES      TO WRK-CAD-MES
               MOVE WRK-DTREF-ANO      TO WRK-CAD-ANO
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
                                          GFCTFK-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFK-COD-MSG-ERRO
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

           MOVE 'N'                    TO WRK-FINALIZAR.

           MOVE ZEROS                  TO IND-1.

           PERFORM 24100-ABRIR-CSR-1.

           PERFORM 24200-LER-CSR-1.

           PERFORM
             UNTIL WRK-FINALIZAR       EQUAL 'S'
                OR IND-1               GREATER 6
                   PERFORM 24400-TRATAR-CAMPOS-CSR-1
           END-PERFORM.

           PERFORM 24300-FECHAR-CSR-1.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE 'N'                TO GFCTFK-FIM
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO WRK-AUX-TARIFA
               MOVE DINIC-VGCIA-TARIF OF GFCTB0A2
                                       TO WRK-AUX-DATA
               MOVE WRK-AUX-PONTEIRO   TO GFCTFK-PONTEIRO
           END-IF.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24100-ABRIR-CSR-1               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB0A2.
           MOVE WRK-AUX-DATA           TO DINIC-VGCIA-TARIF OF GFCTB0A2.
           MOVE WRK-DATA-REFER         TO WRK-DATA.

           EXEC SQL
               OPEN CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFK-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
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
               FETCH CSR-1
               INTO  :GFCTB0A2.CSERVC-TARIF,
                     :GFCTB0A2.DINIC-VGCIA-TARIF,
                     :GFCTB0A2.DFIM-VGCIA-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFK-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFK-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTFK-ERRO
                   MOVE ZEROS          TO GFCTFK-COD-SQL-ERRO
                   MOVE 0443           TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFK-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       24200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24300-FECHAR-CSR-1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFK-COD-SQL-ERRO
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24400-TRATAR-CAMPOS-CSR-1       SECTION.
      *----------------------------------------------------------------*

           PERFORM 24600-OBTER-DADOS-GFCTB0D8.

RIC        IF SQLCODE = 0
RIC           IF (WRK-5507-IDTFD-MASTER   EQUAL 'S') OR
RIC              (WRK-5507-IDTFD-MASTER   EQUAL 'N'  AND
RIC               WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8)
RIC               ADD 1                   TO IND-1
RIC
RIC               IF  IND-1               GREATER 6
RIC                   GO   TO  24400-99-FIM
RIC               END-IF
RIC
RIC               PERFORM 24500-MOVER-CAMPOS
RIC           END-IF
RIC        END-IF.
           PERFORM 24200-LER-CSR-1.

      *----------------------------------------------------------------*
       24400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24500-MOVER-CAMPOS              SECTION.
      *----------------------------------------------------------------*

           COMPUTE GFCTFK-LL           = (GFCTFK-LL + 125).

           MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO GFCTFK-COD-TARIFA(IND-1).
           MOVE DINIC-VGCIA-TARIF OF GFCTB0A2
                                       TO
                                       GFCTFK-DINIC-VGCIA-TARIF(IND-1).
           MOVE DFIM-VGCIA-TARIF OF GFCTB0A2
                                       TO
                                       GFCTFK-DFIM-VGCIA-TARIF(IND-1).
           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTFK-DESC-TARIFA(IND-1).

           ADD 1                       TO GFCTFK-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       24500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24600-OBTER-DADOS-GFCTB0D8      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0A2 TO CSERVC-TARIF  OF GFCTB0D8.

           EXEC SQL
               SELECT  CDEPDC,
                       RSERVC_TARIF_REDZD
               INTO  :GFCTB0D8.CDEPDC,
                     :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM   DB2PRD.SERVC_TARIF_PRINC
RIC            WHERE  CSERVC_TARIF     = :GFCTB0D8.CSERVC-TARIF AND
RIC                   CTPO_SERVC_TARIF <> 2
           END-EXEC.

RIC        IF (SQLCODE                 NOT EQUAL ZEROS AND -811
RIC                                                    AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFK-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 12110-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24600-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
