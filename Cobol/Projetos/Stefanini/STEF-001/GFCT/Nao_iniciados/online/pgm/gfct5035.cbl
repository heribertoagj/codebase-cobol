      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5035.
       AUTHOR. DANIELE MARTINS.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5035                                     *
      *    PROGRAMADORA : DANIELE MARTINS               -  CPM/FPOLIS  *
      *    ANALISTA CPM : MICHELLI VENTURA              -  CPM/FPOLIS  *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK    -  GRUPO 50    *
      *    DATA         : 24/05/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      MODULO DE CONSISTENCIA DOS AGRUPAMENTOS.                  *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.DEPDC_BDSCO_ONLINE              MESUB010          *
      *       DB2PRD.DIR_RGNAL_DEPDC                 MESUB012          *
      *       DB2PRD.HIST_TARIF_PRINC                GFCTB087          *
      *       DB2PRD.TPO_SGMTO_CLI                   CLIEB084          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5507 - VALORIZAR O CAMPO LKG-5507-IDTFD-MASTER        *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      SENH0315 - OBTER JUNCAO                                   *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7742                                     *
      *                                                                *
      *==================  A L T E R A C A O ==========================*
      *    ANALISTA     : WAGNER          - PROCWORK    -  GRUPO 50    *
      *    DATA         : 18/08/2005                                   *
      *                                                                *
      *    OBJETIVO     : INIBIR ACESSO A TABELA CLIEB084              *
      *================================================================*

HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * ***  HEXASOLUTION - 03/2009 - CONVERSAO FAIXA DE AGENCIAS  *** *
HEXA  *----------------------------------------------------------------*
HEXA  *
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
           '*  INICIO DA WORKING GFCT5035  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-SQLCODE                 PIC S9(6)           VALUE ZEROS.
       01  WRK-TIPO-RELAC              PIC 9(03)           VALUE ZEROS.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(11)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA P/ MODULO GFCT5000    *'.
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
           '*   AREA P/ MODULO GFCT5507    *'.
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
           '*   AREA P/ MODULO SENH0315    *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(06)          VALUE
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
ST25X6*     05  WRK-CNPJ-PRI-0315       PIC 9(09)           VALUE ZEROS.
ST25X6*     05  WRK-CNPJ-FIL-0315       PIC 9(05)           VALUE ZEROS.
ST25X6*     05  WRK-CNPJ-CON-0315       PIC 9(02)           VALUE ZEROS.
           05  WRK-CNPJ-PRI-0315       PIC X(09)           VALUE SPACES.
           05  WRK-CNPJ-FIL-0315       PIC X(04)           VALUE SPACES.
           05  WRK-CNPJ-CON-0315       PIC X(02)           VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-SECAO-0315              PIC 9(03)           VALUE ZEROS.
       01  WRK-STATUS-0315             PIC X(01)           VALUE SPACES.

PW    *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MODULO MESU9025 ***'.
      *----------------------------------------------------------------*

       01  WRK-IO-PCB                  PIC X(32)           VALUE SPACES.
       01  WRK-ALT-PCB                 PIC X(32)           VALUE SPACES.
       01  WRK-MENSAGEM.
           03  WRK-MSG-COD             PIC 9(03)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE SPACES.
           03  WRK-MSG-TXT             PIC X(79)           VALUE SPACES.
       01  WRK-RCB-TPO-RELA-X.
           03  WRK-RCB-TPO-RELA        PIC 9(03)           VALUE ZEROS.
       01  WRK-RCB-COD-EMPR-X.
           03  WRK-RCB-COD-EMPR        PIC 9(05)           VALUE ZEROS.
       01  WRK-RCB-COD-JUNC-X.
           03  WRK-RCB-COD-JUNC        PIC 9(05)           VALUE ZEROS.
       01  WRK-RCB-QTD-LIM-X.
           03  WRK-RCB-QTD-LIM         PIC 9(03)           VALUE ZEROS.
       01  WRK-RST-COD-EMPR-X.
           03  WRK-RST-COD-EMPR        PIC 9(05)           VALUE ZEROS.
       01  WRK-RST-COD-JUNC-X.
           03  WRK-RST-COD-JUNC        PIC 9(05)           VALUE ZEROS.
       01  WRK-DEV-QTD-RET             PIC 9(03)           VALUE ZEROS.
       01  WRK-DEV-TAB.
           03  WRK-DEV-TAB-OC          OCCURS 100 TIMES.
               05  WRK-DEV-OC-COD-EMPR PIC 9(05).
               05  WRK-DEV-OC-COD-JUNC PIC 9(05).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   INDICADORES DE NULIDADE    *'.
      *----------------------------------------------------------------*

       01  WRK-MESUB010-NULL.
           03  WRK-CDIR-RGNAL-NULL     PIC S9(04)  COMP    VALUE +0.

       01  1570-ROTINA-DATAS.
           03  1570-DATA.
            05   FILLER           PIC X(2)   VALUE SPACES.
            05   1570-DATA-CONS   PIC 9(8)   VALUE ZEROS.
           03  1570-DATA-R   REDEFINES   1570-DATA PIC X(10).
           03  1570-FORMATO-DATA     PIC 9(2)   VALUE ZEROS.
           03  1570-MENSAGEM         PIC X(80)  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB087
           END-EXEC.

           EXEC SQL
                INCLUDE MESUB011
           END-EXEC.
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY I#HEXA01.
HEXA  *
HEXA       COPY I#BRAD7C.
HEXA  *

      *    EXEC SQL
      *         INCLUDE CLIEB084
      *    END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT5035   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC I#GFCT8A
-INC I#GFCT8B
-INC I#GFCT0M

      *================================================================*
       PROCEDURE DIVISION              USING GFCT8A-ENTRADA
                                             GFCT8B-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
HEXA       MOVE        'BRAD7100'  TO          WRK-ABEND-MESUX0.
HEXA  *

           PERFORM 10000-INICIALIZAR-PROGRAMA.

           PERFORM 20000-PROCESSAR-PROGRAMA.

           PERFORM 30000-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR-PROGRAMA      SECTION.
      *----------------------------------------------------------------*

           PERFORM 11000-INICIALIZAR-SAIDA.

           PERFORM 12000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-INICIALIZAR-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS.

           INITIALIZE                  SQLCA
                                       GFCT8B-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 1010                   TO GFCT8B-LL.
           MOVE GFCT8A-ZZ              TO GFCT8B-ZZ.
           MOVE GFCT8A-TRANSACAO       TO GFCT8B-TRANSACAO.
           MOVE GFCT8A-FUNCAO          TO GFCT8B-FUNCAO.

           IF  GFCT8A-DEPTO-BDSCO      NUMERIC
               MOVE GFCT8A-DEPTO-BDSCO TO GFCT8B-DEPTO-BDSCO
           END-IF.

           MOVE GFCT8A-FUNC-BDSCO      TO GFCT8B-FUNC-BDSCO.
           MOVE GFCT8A-TERMINAL        TO GFCT8B-TERMINAL.
           MOVE GFCT8A-DATA-MMAAAA-BARRA
                                       TO GFCT8B-DATA-MMAAAA-BARRA.

           IF  GFCT8A-COD-AGRUPAMENTO  NUMERIC
               MOVE GFCT8A-COD-AGRUPAMENTO
                                       TO GFCT8B-COD-AGRUPAMENTO
           END-IF.

           MOVE GFCT8A-NOME-AGRUPAMENTO
                                       TO GFCT8B-NOME-AGRUPAMENTO.

           IF  GFCT8A-COD-FILTRO1      NUMERIC
               MOVE GFCT8A-COD-FILTRO1 TO GFCT8B-COD-FILTRO1
           END-IF.

           MOVE GFCT8A-NOME-FILTRO1    TO GFCT8B-NOME-FILTRO1.

           IF  GFCT8A-COD-FILTRO2      NUMERIC
               MOVE GFCT8A-COD-FILTRO2 TO GFCT8B-COD-FILTRO2
           END-IF.

           MOVE GFCT8A-NOME-FILTRO2    TO GFCT8B-NOME-FILTRO2.

********** IF  GFCT8A-COD-EXIBICAO     NUMERIC
**********     MOVE GFCT8A-COD-EXIBICAO
**********                             TO GFCT8B-COD-EXIBICAO
********** END-IF.

********** MOVE GFCT8A-NOME-EXIBICAO   TO GFCT8B-NOME-EXIBICAO.
           MOVE ZEROS                  TO GFCT8B-ERRO
                                          GFCT8B-COD-SQL-ERRO
                                          GFCT8B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           PERFORM 11100-OBTER-DESC-MENSAGEM.

           MOVE WRK-5000-DESC-MSG      TO GFCT8B-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-DESC-MENSAGEM       SECTION.
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
                   MOVE 1              TO GFCT8B-ERRO
                   MOVE ZEROS          TO GFCT8B-COD-SQL-ERRO
                                          GFCT8B-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT8B-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCT8B-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCT8B-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCT8B-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT8B-DESC-MSG-ERRO
               END-IF
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF  GFCT8A-TRANSACAO        EQUAL SPACES  OR
               GFCT8A-FUNCAO           EQUAL SPACES  OR
               GFCT8A-DEPTO-BDSCO      NOT NUMERIC   OR
               GFCT8A-FUNC-BDSCO       EQUAL SPACES  OR
               GFCT8A-DATA-MMAAAA-BARRA
                                       EQUAL SPACES  OR
               GFCT8A-COD-AGRUPAMENTO  NOT NUMERIC
      **       GFCT8A-NOME-AGRUPAMENTO EQUAL SPACES  OR
      **       GFCT8A-COD-FILTRO1      NOT NUMERIC   OR
      **       GFCT8A-NOME-FILTRO1     EQUAL SPACES  OR
CPM************GFCT8A-COD-FILTRO2      NOT NUMERIC   OR
CPM************GFCT8A-NOME-FILTRO2     EQUAL SPACES  OR
      **       GFCT8A-COD-EXIBICAO     NOT NUMERIC   OR
      **       GFCT8A-NOME-EXIBICAO    EQUAL SPACES
               MOVE 1                  TO GFCT8B-ERRO
               MOVE ZEROS              TO GFCT8B-COD-SQL-ERRO
               MOVE 0001               TO GFCT8B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCT8B-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

            MOVE GFCT8A-DATA-MMAAAA-BARRA  TO  1570-DATA-CONS.
            MOVE 13        TO  1570-FORMATO-DATA.

            CALL  'POOL1570'  USING  1570-DATA-R
                              1570-FORMATO-DATA
                              1570-MENSAGEM.

            IF   RETURN-CODE   NOT EQUAL   ZERO
                 MOVE 1                  TO GFCT8B-ERRO
                 MOVE ZEROS              TO GFCT8B-COD-SQL-ERRO
                 MOVE 0077               TO GFCT8B-COD-MSG-ERRO
                                         WRK-5000-COD-MSG
                 PERFORM 11100-OBTER-DESC-MENSAGEM
                 MOVE WRK-5000-DESC-MSG  TO GFCT8B-DESC-MSG-ERRO
                 PERFORM 30000-FINALIZAR-PROGRAMA
            END-IF.
      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR-PROGRAMA        SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-OBTER-COD-JUNCAO.

           PERFORM 22000-VERIFICAR-MASTER.

      *----------------->>> MASTER
           IF  WRK-5507-IDTFD-MASTER   EQUAL 'S'
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------->>> DEPTO DO USUARIO = DEPTO A ACESSAR
           IF ((GFCT8A-COD-AGRUPAMENTO = 5 OR 11 OR 12 OR 14 OR 15) AND
               (GFCT8A-DEPTO-BDSCO     = GFCT8A-COD-FILTRO1))
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------->>> AGENCIA
           IF  GFCT8A-COD-AGRUPAMENTO = 5 OR 14 OR 15
               MOVE 17            TO WRK-TIPO-RELAC
               PERFORM 20100-VERIFICAR-DIRET-GER
               IF  SQLCODE     EQUAL ZERO   OR -811
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               MOVE 11            TO WRK-TIPO-RELAC
               PERFORM 20100-VERIFICAR-DIRET-GER
               IF  SQLCODE     EQUAL ZERO   OR -811
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
           END-IF.

      *----------------->>> GERENTE REGIONAL
           IF  GFCT8A-COD-AGRUPAMENTO = 11
               MOVE 11            TO WRK-TIPO-RELAC
               PERFORM 20100-VERIFICAR-DIRET-GER
               IF  SQLCODE     EQUAL ZERO  OR  -811
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
           END-IF.

      *----------------->>> DIRETORIA REGIONAL
      *    IF  GFCT8A-COD-AGRUPAMENTO = 12
      *        MOVE 17            TO WRK-TIPO-RELAC
      *        PERFORM 20100-VERIFICAR-DIRET-GER
      *        IF  SQLCODE     EQUAL ZERO  OR  -811
      *            PERFORM 30000-FINALIZAR-PROGRAMA
      *        END-IF
      *    END-IF.

      *----------------->>> SEGMENTO
      *    IF  GFCT8A-COD-AGRUPAMENTO = 9
      *        PERFORM 25000-PESQUISAR-SEGMENTO
      *        IF  SQLCODE     EQUAL ZERO  OR  -811
      *            PERFORM 30000-FINALIZAR-PROGRAMA
      *        END-IF
      *    END-IF.

      *----------------->>> TARIFA
           IF  GFCT8A-COD-AGRUPAMENTO = 1
               PERFORM 26000-PESQUISAR-TARIFAS
               IF  SQLCODE     EQUAL ZERO  OR  -811
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
           END-IF.

      *----------------->>> CONDICAO DE ERRO
           MOVE 1                 TO GFCT8B-ERRO
           MOVE ZEROS             TO GFCT8B-COD-SQL-ERRO
           MOVE 0213              TO GFCT8B-COD-MSG-ERRO
                                     WRK-5000-COD-MSG
           PERFORM 11100-OBTER-DESC-MENSAGEM
           MOVE WRK-5000-DESC-MSG TO GFCT8B-DESC-MSG-ERRO
           PERFORM 30000-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20100-VERIFICAR-DIRET-GER       SECTION.
      *----------------------------------------------------------------*

      *--------------->> SO PARA DEPARTAMENTOS
HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  *    HX_IF      GFCT8A-DEPTO-BDSCO  <  4000
HEXA  *
HEXA          MOVE     GFCT8A-DEPTO-BDSCO TO   WRK-MESUX0-AGENCIA
HEXA          PERFORM  9990-10-CHAMA-ROTINA-FX-AG
HEXA  *
HEXA          IF      WRK-EH-FX-AG
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  *
                      MOVE +100          TO  SQLCODE
                      GO                 TO  20100-99-FIM.

      *--------------->> VERIFICA SE USUARIO E GER OU DIRET REGIONAL
              MOVE    WRK-TIPO-RELAC     TO  CTPO-DEPDC-RLCDA
              MOVE    GFCT8A-DEPTO-BDSCO TO  CDEPDC-RLCDA
              MOVE    GFCT8A-COD-FILTRO1 TO  CDEPDC.

              EXEC SQL
                SELECT  CEMPR_INC,
                        CDEPDC
                INTO    :CEMPR-INC,
                        :CDEPDC
                FROM  DB2PRD.DEPDC_RLCDA
                  WHERE  CTPO_DEPDC_RLCDA = :CTPO-DEPDC-RLCDA
                    AND  CDEPDC_RLCDA     = :CDEPDC-RLCDA
                    AND  CDEPDC           = :CDEPDC
                    AND  CEMPR_INC_RLCDA  = 237
                    AND  CEMPR_INC        = 237
              END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
               SQLWARN0                EQUAL 'W'
               MOVE 'GFCT5035'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'DEPDC_DEPDC_RLCDA '
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-STACODE
                                          GFCT8B-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT8B-ERRO
               MOVE 0012               TO GFCT8B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE ' - MESUB011'      TO WRK-NOME-TAB
               PERFORM 23100-CONCATENAR-MSG-DB2
               PERFORM 30000-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       20100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCT8A-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                  TO GFCT8B-ERRO
               MOVE RETURN-CODE        TO GFCT8B-COD-SQL-ERRO
               MOVE 0056               TO GFCT8B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCT8B-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 001
                   MOVE 1              TO GFCT8B-ERRO
                   MOVE ZEROS          TO GFCT8B-COD-SQL-ERRO
                   MOVE 0057           TO GFCT8B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT8B-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 154
                   MOVE 1              TO GFCT8B-ERRO
                   MOVE ZEROS          TO GFCT8B-COD-SQL-ERRO
                   MOVE 0058           TO GFCT8B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT8B-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 242
                   MOVE 1              TO GFCT8B-ERRO
                   MOVE ZEROS          TO GFCT8B-COD-SQL-ERRO
                   MOVE 0059           TO GFCT8B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT8B-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 009
                   MOVE ZEROS          TO GFCT8B-ERRO
                                          GFCT8B-COD-SQL-ERRO
                                          GFCT8B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT8B-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 138
                   MOVE 1              TO GFCT8B-ERRO
                   MOVE ZEROS          TO GFCT8B-COD-SQL-ERRO
                   MOVE 0060           TO GFCT8B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT8B-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
           END-IF.

           IF  GFCT8B-DEPTO-BDSCO      EQUAL 0  OR
               GFCT8A-DEPTO-BDSCO      EQUAL 0
               MOVE WRK-COD-JUNCAO-0315   TO GFCT8B-DEPTO-BDSCO
                                             GFCT8A-DEPTO-BDSCO
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
                   MOVE 1              TO GFCT8B-ERRO
                   MOVE ZEROS          TO GFCT8B-COD-SQL-ERRO
                                          GFCT8B-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCT8B-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                       MOVE 1          TO GFCT8B-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCT8B-COD-SQL-ERRO
                       MOVE ZEROS      TO GFCT8B-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                       TO GFCT8B-DESC-MSG-ERRO
                   END-IF
               END-IF
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23100-CONCATENAR-MSG-DB2        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT8B-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO GFCT8B-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       23100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *25000-PESQUISAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*
      *
      *    MOVE GFCT8A-DEPTO-BDSCO     TO CJUNC-DEPDC-SGMTO OF CLIEB084.
      *    MOVE GFCT8A-COD-FILTRO1     TO CSGMTO-CLI        OF CLIEB084.
      *
      *    EXEC SQL
      *        SELECT CSGMTO_CLI
      *        INTO  :CLIEB084.CSGMTO-CLI
      *        FROM   DB2PRD.TPO_SGMTO_CLI
      *        WHERE  CSGMTO_CLI       = :CLIEB084.CSGMTO-CLI
      *          AND  CJUNC_DEPDC_SGMTO
      *                                = :CLIEB084.CJUNC-DEPDC-SGMTO
      *    END-EXEC
      *
      *    IF (SQLCODE                 NOT EQUAL ZEROS AND +100
      *                                                AND -811) OR
      *        SQLWARN0                EQUAL 'W'
      *        MOVE 'GFCT5035'         TO GFCT0M-PROGRAMA
      *        MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
      *        MOVE 'TPO_SGMTO_CLI'    TO GFCT0M-NOME-TAB
      *        MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
      *        MOVE  SQLCODE           TO GFCT0M-STACODE
      *                                   GFCT8B-COD-SQL-ERRO
      *        MOVE '0004'             TO GFCT0M-LOCAL
      *        MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
      *        MOVE  9                 TO GFCT8B-ERRO
      *        MOVE  0012              TO GFCT8B-COD-MSG-ERRO
      *                                   WRK-5000-COD-MSG
      *        PERFORM 11100-OBTER-DESC-MENSAGEM
      *        MOVE ' - CLIEB084'      TO WRK-NOME-TAB
      *        PERFORM 23100-CONCATENAR-MSG-DB2
      *        PERFORM 30000-FINALIZAR-PROGRAMA
      *    ELSE
      *        IF  SQLCODE             EQUAL +100
      *            MOVE ZEROS          TO GFCT8B-ERRO
      *            MOVE SQLCODE        TO GFCT8B-COD-SQL-ERRO
      *            MOVE 0010           TO GFCT8B-COD-MSG-ERRO
      *                                   WRK-5000-COD-MSG
      *            PERFORM 11100-OBTER-DESC-MENSAGEM
      *            MOVE ' - CLIEB084'  TO WRK-NOME-TAB
      *            PERFORM 23100-CONCATENAR-MSG-DB2
      *            PERFORM 30000-FINALIZAR-PROGRAMA
      *        END-IF
      *    END-IF.
      *
      *----------------------------------------------------------------*
      *25000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26000-PESQUISAR-TARIFAS         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT8A-COD-FILTRO1     TO CSERVC-TARIF OF GFCTB087.
           MOVE WRK-COD-JUNCAO-0315    TO CDEPDC-GSTAO-SERVC
                                                           OF GFCTB087.

           EXEC SQL
                SELECT CDEPDC_GSTAO_SERVC
                INTO  :GFCTB087.CEMPR-GSTAO-SERVC
                FROM   DB2PRD.HIST_TARIF_PRINC
                WHERE  CSERVC_TARIF    = :GFCTB087.CSERVC-TARIF AND
                       CDEPDC_GSTAO_SERVC
                                       = :GFCTB087.CDEPDC-GSTAO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
               SQLWARN0                EQUAL 'W'
               MOVE 'GFCT5035'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-STACODE
                                          GFCT8B-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT8B-ERRO
               MOVE  0012              TO GFCT8B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB087'      TO WRK-NOME-TAB
               PERFORM 23100-CONCATENAR-MSG-DB2
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       26000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR-PROGRAMA        SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
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
HEXA       IF          WRK-EH-ERRO-MESU-ABEND
HEXA           MOVE    'APL'       TO          ERR-TIPO-ACESSO
HEXA           MOVE    'ERRO NA CHAMADA DA ROTINA DE FAIXAS DE AGENCIA'
HEXA             TO    ERR-TEXTO
HEXA           PERFORM 9991-10-CHAMA-ROTINA-ABEND
HEXA       ELSE
HEXA           MOVE    ZEROS       TO          WRK-MESUX0-AGENCIA
HEXA       END-IF.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE ABEND
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-10-CHAMA-ROTINA-ABEND SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       MOVE    'GFCT5035'      TO          ERR-PGM.
HEXA  *
HEXA       CALL    WRK-ABEND-MESUX0 USING      WRK-BATCH-MESUX0
HEXA                                           ERRO-AREA.
HEXA  *
HEXA       GOBACK.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
