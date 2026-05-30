      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0386.
       AUTHOR. CARLOS EDUARDO ARDIGO.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0386                                     *
      *    PROGRAMADOR  : CARLOS EDUARDO ARDIGO         - CPM/FPOLIS   *
      *    ANALISTA CPM : RODRIGO WILLAIM               - CPM/FPOLIS   *
      *    ANALISTA     : MARCELO - PROCWORK            - GRUPO 50     *
      *    DATA         : 07/08/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      LISTA DE ESTORNOS DE AGRUPAMENTOS - PARA CONSULTA.        *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.PARM_DATA_PROCM                 GFCTB0A1          *
      *       DB2PRD.PDIDO_ESTRN_AG                  GFCTB0A3          *
      *       DB2PRD.PDIDO_ESTRN_CLI                 GFCTB0A4          *
      *       DB2PRD.PDIDO_ESTRN_EMPR                GFCTB0A6          *
      *       DB2PRD.PDIDO_ESTRN_GRP                 GFCTB0A7          *
      *       DB2PRD.PDIDO_ESTRN_PAB                 GFCTB0A8          *
      *       DB2PRD.PDIDO_ESTRN_PSTAL               GFCTB0A9          *
      *       DB2PRD.PDIDO_ESTRN_SGMTO               GFCTB0B0          *
      *       DB2PRD.TTPO_SGMTO_GSTAO                GFCTB0H5          *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0D8          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5500 - VERIFICACAO DE DISPONIBILIDADE ON-LINE         *
      *      GFCT5006 - OBTER DESCRICAO DA AGENCIA                     *
      *      GFCT5090 - OBTER DESCRICAO DO CORRESPONDENTE BANCARIO     *
      *      GFCT5109 - OBTER DESCRICAO DO PAB                         *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7385                                     *
      *                                                                *
      *================================================================*
      *                                                                *
      *   ULTIMA ALTERACAO: MARCELO CREMM - PROCWORK - SETEMBRO 2006   *
      *   ==========================================================   *
      *                                                                *
      *   EQUALIZAR TRATAMENTO DE SITUACAO IDENTICO AO DO AG/CTA.      *
      *                                                                *
      *   ALTERACOES COM PREFIXO PWIE.                                 *
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
           '*  INICIO DA WORKING GFCT0386  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND                         PIC 9(06)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-FINALIZAR               PIC X(01)           VALUE SPACES.
       01  WRK-CONTA                   PIC X(01)           VALUE SPACES.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.
       01  WRK-TARIFA                  PIC S9(05)          VALUE ZEROS.

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
           'ERRO NA CHAMADA DO MODULO '.
           03  WRK-MSG05-MODULO        PIC X(08)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS DE NULIDADE    *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB0B0-NULL.
           03  WRK-CSGMTO-GSTAO-TARIF-NULL
                                       PIC S9(04) COMP     VALUE +0.

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
           '*    AREA DO MODULO GFCT5006   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1V'.
       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5090   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGB'.
       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5109   *'.
      *----------------------------------------------------------------*

      *COPY I#GFCTGB.
      *COPY I#GFCTGC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A4
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A6
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A7
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A9
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
               DECLARE CSR1-GFCTB0A7 CURSOR FOR
                SELECT CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CSIT_SOLTC_ESTRN,
                       CSIT_PROCM_ESTRN,
                       CINDCD_AGPTO_TOT,
                       CDEPDC_SOLTC_ESTRN,
                       CFUNC_SOLTC_ESTRN,
                       HSOLTC_ESTRN,
                       CTPO_OPER_MOTVO,
                       CMOTVO_JUSTF,
                       RJUSTF_SOLTC_ESTRN,
                       CDEPDC_PAREC_ESTRN,
                       CFUNC_PAREC_ESTRN,
                       HPAREC_ESTRN,
                       RJUSTF_PAREC_ESTRN,
                       QESTRN_AGPTO,
                       VESTRN_SOLTD,
                       QCANCT_AGPTO,
                       VCANCT_SOLTD,
                       DEFETV_CREDT_ESTRN
                FROM   DB2PRD.PDIDO_ESTRN_GRP
               WHERE  ((CAGPTO_CTA      = :GFCTB0A7.CAGPTO-CTA
                 AND    CSERVC_TARIF    = :GFCTB0A7.CSERVC-TARIF
                 AND    DOCOR_EVNTO     = :GFCTB0A7.DOCOR-EVNTO
                 AND    HIDTFD_ESTRN_AGPTO
                                       >= :GFCTB0A7.HIDTFD-ESTRN-AGPTO)
                  OR   (CAGPTO_CTA      = :GFCTB0A7.CAGPTO-CTA
                 AND    CSERVC_TARIF    = :GFCTB0A7.CSERVC-TARIF
                 AND    DOCOR_EVNTO     > :GFCTB0A7.DOCOR-EVNTO)
                  OR   (CAGPTO_CTA      = :GFCTB0A7.CAGPTO-CTA
                 AND    CSERVC_TARIF    > :GFCTB0A7.CSERVC-TARIF))
            ORDER BY   CSERVC_TARIF,
                       DOCOR_EVNTO,
0808                   HIDTFD_ESTRN_AGPTO
           END-EXEC.

           EXEC SQL
               DECLARE CSR2-GFCTB0A7 CURSOR FOR
                SELECT CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CSIT_SOLTC_ESTRN,
                       CSIT_PROCM_ESTRN,
                       CINDCD_AGPTO_TOT,
                       CDEPDC_SOLTC_ESTRN,
                       CFUNC_SOLTC_ESTRN,
                       HSOLTC_ESTRN,
                       CTPO_OPER_MOTVO,
                       CMOTVO_JUSTF,
                       RJUSTF_SOLTC_ESTRN,
                       CDEPDC_PAREC_ESTRN,
                       CFUNC_PAREC_ESTRN,
                       HPAREC_ESTRN,
                       RJUSTF_PAREC_ESTRN,
                       QESTRN_AGPTO,
                       VESTRN_SOLTD,
                       QCANCT_AGPTO,
                       VCANCT_SOLTD,
                       DEFETV_CREDT_ESTRN
                FROM   DB2PRD.PDIDO_ESTRN_GRP
               WHERE  ((CAGPTO_CTA      = :GFCTB0A7.CAGPTO-CTA
                 AND    CSERVC_TARIF    = :GFCTB0A7.CSERVC-TARIF
                 AND    DOCOR_EVNTO     = :GFCTB0A7.DOCOR-EVNTO
                 AND    HIDTFD_ESTRN_AGPTO
                                       >= :GFCTB0A7.HIDTFD-ESTRN-AGPTO)
                  OR   (CAGPTO_CTA      = :GFCTB0A7.CAGPTO-CTA
                 AND    CSERVC_TARIF    = :GFCTB0A7.CSERVC-TARIF
                 AND    DOCOR_EVNTO     > :GFCTB0A7.DOCOR-EVNTO))
            ORDER BY   DOCOR_EVNTO,
0808                   HIDTFD_ESTRN_AGPTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT0386   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGL'.
       COPY 'I#GFCTGM'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTGL-ENTRADA
                                             GFCTGM-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       0000000-ROTINA-PRINCIPAL        SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000000-INICIALIZAR.

           PERFORM 2000000-PROCESSAR.

           PERFORM 3000000-FINALIZAR.

      *----------------------------------------------------------------*
       0000000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000000-INICIALIZAR             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGM-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGM-SAIDA
                                       GFCT0M-AREA-ERROS
                                       SQLCA.

           MOVE 290                    TO GFCTGM-LL.
           MOVE GFCTGL-TRANSACAO       TO GFCTGM-TRANSACAO.
           MOVE GFCTGL-FUNCAO          TO GFCTGM-FUNCAO.
           MOVE GFCTGL-FUNC-BDSCO      TO GFCTGM-FUNC-BDSCO.
           MOVE GFCTGL-FILTRO          TO GFCTGM-FILTRO.
           MOVE GFCTGL-PONTEIRO        TO GFCTGM-PONTEIRO.
           MOVE ZEROS                  TO GFCTGM-ERRO
                                          GFCTGM-COD-SQL-ERRO
                                          GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.
           MOVE 'N'                    TO GFCTGM-FIM
                                          WRK-FINALIZAR.

           IF  GFCTGL-QTDE-OCOR        IS NUMERIC
               MOVE GFCTGL-QTDE-OCOR   TO GFCTGM-QTDE-OCOR
           ELSE
               MOVE ZEROS              TO GFCTGM-QTDE-OCOR
           END-IF.

           IF  GFCTGL-QTDE-TOT-REG     IS NUMERIC
               MOVE GFCTGL-QTDE-TOT-REG
                                       TO GFCTGM-QTDE-TOT-REG
           ELSE
               MOVE ZEROS              TO GFCTGM-QTDE-TOT-REG
           END-IF.

           PERFORM 1100000-OBTER-DESC-MSG.

           MOVE WRK-5000-DESC-MSG      TO GFCTGM-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1000000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100000-OBTER-DESC-MSG          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5000-AREA-RECEBE.

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE ZEROS          TO GFCTGM-COD-SQL-ERRO
                                          GFCTGM-COD-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTGM-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTGM-COD-MSG-ERRO
               END-IF
               MOVE 1                  TO GFCTGM-ERRO
               MOVE 'S'                TO GFCTGM-FIM
               MOVE WRK-5000-DESC-MSG  TO GFCTGM-DESC-MSG-ERRO
               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000000-PROCESSAR               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100000-CONSISTIR-CAMPOS.

           PERFORM 2200000-ACESSAR-GFCTB0A1.

           PERFORM 2300000-OBTER-LISTA.

      *----------------------------------------------------------------*
       2000000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100000-CONSISTIR-CAMPOS        SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110000-VERIFICAR-ONLINE.

           IF (GFCTGL-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTGL-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTGL-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTGL-FILTRO-AGPTO     NOT NUMERIC)                OR
              (GFCTGL-FILTRO-AGPTO     EQUAL ZEROS)                OR
              (GFCTGL-FILTRO-AGPTO     NOT EQUAL 4 AND 14 AND 9
                                       AND 15 AND 16 AND 17)       OR
              (GFCTGL-FILTRO-TARIFA    NOT NUMERIC)                OR
              (GFCTGL-PONT-AGPTO       NOT NUMERIC)                OR
              (GFCTGL-PONT-AGPTO       EQUAL ZEROS)                OR
              (GFCTGL-PONT-AGPTO       NOT EQUAL 4 AND 14 AND 9
                                       AND 15 AND 16 AND 17)       OR
              (GFCTGL-PONT-TARIFA      NOT NUMERIC)
               MOVE 1                  TO GFCTGM-ERRO
               MOVE ZEROS              TO GFCTGM-COD-SQL-ERRO
               MOVE 0001               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTGM-FIM
               PERFORM 1100000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTGM-DESC-MSG-ERRO
               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110000-VERIFICAR-ONLINE        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5500-AREA-SAIDA
                                       SQLCA.

           MOVE 'GFCT5500'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5500-AREA-SAIDA.

           IF  WRK-5500-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 04
                   MOVE 1              TO GFCTGM-ERRO
                   MOVE 'S'            TO GFCTGM-FIM
                   MOVE ZEROS          TO GFCTGM-COD-SQL-ERRO
                   MOVE 0030           TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 1100000-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGM-DESC-MSG-ERRO
               END-IF
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   INITIALIZE          GFCT0M-AREA-ERROS
                   MOVE 'PARM_CTRL_GSTAO'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
                   MOVE '0001'         TO GFCT0M-LOCAL
                   MOVE 0012           TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'GFCT0386'     TO GFCT0M-PROGRAMA
                   MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
                   MOVE SQLCA          TO GFCT0M-SQLCA-AREA
                   MOVE 9              TO GFCTGM-ERRO
                   MOVE WRK-5500-DESC-MSG
                                       TO GFCTGM-DESC-MSG-ERRO
               END-IF
               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200000-ACESSAR-GFCTB0A1        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
               SELECT  CSIST_PRINC,
                       DPROCM_ANTER,
                       DPROCM_ATUAL,
                       DPROCM_PROX
               INTO    :GFCTB0A1.CSIST-PRINC,
                       :GFCTB0A1.DPROCM-ANTER,
                       :GFCTB0A1.DPROCM-ATUAL,
                       :GFCTB0A1.DPROCM-PROX
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210000-MOVER-ERRO-SQL          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0386'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO GFCTGM-ERRO.

           PERFORM 1100000-OBTER-DESC-MSG.

           MOVE SPACES                 TO GFCTGM-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO GFCTGM-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2210000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300000-OBTER-LISTA             SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGL-FILTRO-TARIFA    EQUAL ZEROS
               PERFORM 2310000-ABRIR-CSR1-GFCTB0A7

               PERFORM 2320000-LER-CSR1-GFCTB0A7

               MOVE ZEROS                  TO IND

               PERFORM 2330000-PROCESSAR-CURSOR
                                       UNTIL WRK-FINALIZAR EQUAL 'S'

               PERFORM 2340000-FECHAR-CSR1-GFCTB0A7
           ELSE
               PERFORM 2350000-ABRIR-CSR2-GFCTB0A7

               PERFORM 2360000-LER-CSR2-GFCTB0A7

               MOVE ZEROS                  TO IND

               PERFORM 2330000-PROCESSAR-CURSOR
                                       UNTIL WRK-FINALIZAR EQUAL 'S'

               PERFORM 2370000-FECHAR-CSR2-GFCTB0A7
           END-IF.

      *----------------------------------------------------------------*
       2300000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310000-ABRIR-CSR1-GFCTB0A7     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGL-PONT-AGPTO       NOT EQUAL ZEROS
               MOVE GFCTGL-PONT-AGPTO  TO CAGPTO-CTA       OF GFCTB0A7
           ELSE
               MOVE GFCTGL-FILTRO-AGPTO
                                       TO CAGPTO-CTA       OF GFCTB0A7
           END-IF.

           IF  GFCTGL-PONT-TARIFA      NOT EQUAL ZEROS
               MOVE GFCTGL-PONT-TARIFA TO CSERVC-TARIF     OF GFCTB0A7
           ELSE
               MOVE GFCTGL-FILTRO-TARIFA
                                       TO CSERVC-TARIF     OF GFCTB0A7
           END-IF.

           IF  GFCTGL-PONT-DATA        EQUAL SPACES OR LOW-VALUES
               MOVE '01.01.0001'       TO DOCOR-EVNTO      OF GFCTB0A7
           ELSE
               MOVE GFCTGL-PONT-DATA   TO DOCOR-EVNTO      OF GFCTB0A7
           END-IF.

           IF  GFCTGL-PONT-TIMESTAMP   EQUAL SPACES OR LOW-VALUES
               MOVE '0001-01-01-01.01.01.000001'
                                       TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A7
           ELSE
               MOVE GFCTGL-PONT-TIMESTAMP
                                       TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A7
           END-IF.

           EXEC SQL
               OPEN CSR1-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2310000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2320000-LER-CSR1-GFCTB0A7       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR1-GFCTB0A7
               INTO    :GFCTB0A7.CAGPTO-CTA,
                       :GFCTB0A7.CSERVC-TARIF,
                       :GFCTB0A7.DOCOR-EVNTO,
                       :GFCTB0A7.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A7.CSIT-SOLTC-ESTRN,
                       :GFCTB0A7.CSIT-PROCM-ESTRN,
                       :GFCTB0A7.CINDCD-AGPTO-TOT,
                       :GFCTB0A7.CDEPDC-SOLTC-ESTRN,
                       :GFCTB0A7.CFUNC-SOLTC-ESTRN,
                       :GFCTB0A7.HSOLTC-ESTRN,
                       :GFCTB0A7.CTPO-OPER-MOTVO,
                       :GFCTB0A7.CMOTVO-JUSTF,
                       :GFCTB0A7.RJUSTF-SOLTC-ESTRN,
                       :GFCTB0A7.CDEPDC-PAREC-ESTRN,
                       :GFCTB0A7.CFUNC-PAREC-ESTRN,
                       :GFCTB0A7.HPAREC-ESTRN,
                       :GFCTB0A7.RJUSTF-PAREC-ESTRN,
                       :GFCTB0A7.QESTRN-AGPTO,
                       :GFCTB0A7.VESTRN-SOLTD,
                       :GFCTB0A7.QCANCT-AGPTO,
                       :GFCTB0A7.VCANCT-SOLTD,
                       :GFCTB0A7.DEFETV-CREDT-ESTRN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTGM-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTGM-ERRO
                   MOVE ZEROS          TO GFCTGM-COD-SQL-ERRO
                   MOVE 0484           TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 1100000-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGM-DESC-MSG-ERRO
                   PERFORM 3000000-FINALIZAR
               END-IF
               GO                      TO 2320000-99-FIM
           END-IF.

           IF  CSERVC-TARIF OF GFCTB0A7
                                       NOT EQUAL WRK-TARIFA
               PERFORM 2321000-ACESSAR-GFCTB0D8
               MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO WRK-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       2320000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2321000-ACESSAR-GFCTB0D8        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT  CDEPDC,
                       RSERVC_TARIF_REDZD
               INTO    :GFCTB0D8.CDEPDC,
                       :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF  = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2321000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2330000-PROCESSAR-CURSOR        SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO IND.
           ADD 180                     TO GFCTGM-LL.

PWIE       PERFORM 2330001-IDENTIFICAR-SITUACAO.
PWIE
           PERFORM 2331000-MONTAR-OCORR-SAIDA.


           IF  GFCTGL-FILTRO-TARIFA    EQUAL ZEROS
               PERFORM 2320000-LER-CSR1-GFCTB0A7
           ELSE
               PERFORM 2360000-LER-CSR2-GFCTB0A7
           END-IF.

           IF  IND                     EQUAL 4 AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE 'S'                TO WRK-FINALIZAR
               MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO GFCTGM-PONT-AGPTO
               MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO GFCTGM-PONT-TARIFA
               MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO GFCTGM-PONT-DATA
               MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO GFCTGM-PONT-TIMESTAMP
           END-IF.

      *----------------------------------------------------------------*
       2330000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

PWIE  *----------------------------------------------------------------*
PWIE   2330001-IDENTIFICAR-SITUACAO    SECTION.
PWIE  *----------------------------------------------------------------*
PWIE
PWIE       IF CSIT-PROCM-ESTRN OF GFCTB0A7
PWIE                                   GREATER 3
PWIE          MOVE 4                   TO GFCTGM-SIT(IND)
PWIE       ELSE
PWIE          IF CSIT-SOLTC-ESTRN OF GFCTB0A7
PWIE                                   EQUAL 1 OR 4
PWIE             MOVE 3                TO GFCTGM-SIT(IND)
PWIE          ELSE
PWIE             IF CSIT-SOLTC-ESTRN OF GFCTB0A7
PWIE                                   EQUAL 3
PWIE                MOVE 2             TO GFCTGM-SIT(IND)
PWIE             ELSE
PWIE                MOVE 1             TO GFCTGM-SIT(IND)
PWIE             END-IF
PWIE          END-IF
PWIE       END-IF.
PWIE
PWIE  *----------------------------------------------------------------*
PWIE   2330001-99-FIM.                 EXIT.
PWIE  *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331000-MONTAR-OCORR-SAIDA      SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTGL-PONT-AGPTO
              WHEN 15
                  PERFORM 2331100-OBTER-PAB
              WHEN 14
                  PERFORM 2331200-OBTER-AGENCIA
              WHEN 16
                  PERFORM 2331300-OBTER-EMPRESA
              WHEN 17
                  PERFORM 2331400-OBTER-POSTAL
              WHEN 4
                  PERFORM 2331500-OBTER-CLIENTE
              WHEN OTHER
                  PERFORM 2331600-OBTER-SEGMENTO
           END-EVALUATE.

           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO GFCTGM-TARIFA(IND).
           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTGM-DESC-TARIFA(IND).
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO GFCTGM-DATA(IND).
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO GFCTGM-HORA(IND).
           MOVE VESTRN-SOLTD       OF GFCTB0A7
                                       TO GFCTGM-VALOR(IND).
PWIS*******MOVE CSIT-PROCM-ESTRN   OF GFCTB0A7
PWIS*******                            TO GFCTGM-SIT(IND).
           MOVE CDEPDC-SOLTC-ESTRN OF GFCTB0A7
                                       TO GFCTGM-DEP(IND).

           ADD 1                       TO GFCTGM-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       2331000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331100-OBTER-PAB               SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA      OF GFCTB0A8.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF    OF GFCTB0A8.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO     OF GFCTB0A8.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                          OF GFCTB0A8.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO    :GFCTB0A8.CAGPTO-CTA,
                       :GFCTB0A8.CSERVC-TARIF,
                       :GFCTB0A8.DOCOR-EVNTO,
                       :GFCTB0A8.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A8.CEMPR-INC,
                       :GFCTB0A8.CDEPDC,
                       :GFCTB0A8.CPOSTO-SERVC
               FROM    DB2PRD.PDIDO_ESTRN_PAB
               WHERE   CAGPTO_CTA         = :GFCTB0A8.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A8.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A8.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A8.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_PAB'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A8'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

           MOVE CDEPDC       OF GFCTB0A8
                                       TO GFCTGM-AGENCIA(IND).
           MOVE CPOSTO-SERVC OF GFCTB0A8
                                       TO GFCTGM-POSTO(IND).

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTGM-DESC-NOME(IND)
           ELSE
               PERFORM 2331110-OBTER-DESC-PAB

               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTGM-DESC-NOME(IND)
           END-IF.

      *----------------------------------------------------------------*
       2331100-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331110-OBTER-DESC-PAB          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTGL-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTGL-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTGL-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC       OF GFCTB0A8
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB0A8
                                       TO GFCTGB-COD-POSTO.

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTGM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0386'         TO GFCT0M-TRANSACAO

               PERFORM 3000000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGM-FIM
               MOVE GFCTGC-ERRO        TO GFCTGM-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTGM-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTGM-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTGM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGM-DESC-MSG-ERRO
               END-IF

               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2331110-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331200-OBTER-AGENCIA           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA   OF GFCTB0A3.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0A3.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB0A3.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                       OF GFCTB0A3.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CEMPR_INC,
                       CDEPDC
               INTO    :GFCTB0A3.CAGPTO-CTA,
                       :GFCTB0A3.CSERVC-TARIF,
                       :GFCTB0A3.DOCOR-EVNTO,
                       :GFCTB0A3.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A3.CEMPR-INC,
                       :GFCTB0A3.CDEPDC
               FROM    DB2PRD.PDIDO_ESTRN_AG
               WHERE   CAGPTO_CTA         = :GFCTB0A3.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A3.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A3.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A3.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_AG'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A3'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTGM-DESC-NOME(IND)
           ELSE
               MOVE CDEPDC OF GFCTB0A3 TO GFCTGM-AGENCIA(IND)

               PERFORM 2331210-OBTER-DESC-AGENCIA

               MOVE GFCT1X-DESC-DEPDC(1)
                                       TO GFCTGM-DESC-NOME(IND)
           END-IF.

      *----------------------------------------------------------------*
       2331200-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331210-OBTER-DESC-AGENCIA      SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.

           MOVE GFCTGL-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTGL-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTGL-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE CDEPDC OF GFCTB0A3     TO GFCT1V-COD-DEPDC(1).

           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT1X-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTGM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0386'         TO GFCT0M-TRANSACAO

               PERFORM 3000000-FINALIZAR
           END-IF

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGM-FIM
               MOVE GFCT1X-ERRO        TO GFCTGM-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTGM-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTGM-COD-MSG-ERRO

               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTGM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'
                          GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGM-DESC-MSG-ERRO
               END-IF

               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2331210-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331300-OBTER-EMPRESA           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA   OF GFCTB0A6.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0A6.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB0A6.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                       OF GFCTB0A6.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CAG_EMPR,
                       CRZ_CTA_EMPR,
                       CNRO_CTA_DEB,
                       CLCTO_CTA_EMPR
               INTO    :GFCTB0A6.CAGPTO-CTA,
                       :GFCTB0A6.CSERVC-TARIF,
                       :GFCTB0A6.DOCOR-EVNTO,
                       :GFCTB0A6.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A6.CAG-EMPR,
                       :GFCTB0A6.CRZ-CTA-EMPR,
                       :GFCTB0A6.CNRO-CTA-DEB,
                       :GFCTB0A6.CLCTO-CTA-EMPR
               FROM    DB2PRD.PDIDO_ESTRN_EMPR
               WHERE   CAGPTO_CTA         = :GFCTB0A6.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A6.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A6.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A6.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_EMPR' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0008'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A6'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

           MOVE CAG-EMPR     OF GFCTB0A6
                                       TO GFCTGM-AGENCIA(IND).
           MOVE CNRO-CTA-DEB OF GFCTB0A6
                                       TO GFCTGM-CONTA(IND).

      *----------------------------------------------------------------*
       2331300-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331400-OBTER-POSTAL            SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA   OF GFCTB0A9.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0A9.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB0A9.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                       OF GFCTB0A9.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO    :GFCTB0A9.CAGPTO-CTA,
                       :GFCTB0A9.CSERVC-TARIF,
                       :GFCTB0A9.DOCOR-EVNTO,
                       :GFCTB0A9.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A9.CEMPR-INC,
                       :GFCTB0A9.CDEPDC,
                       :GFCTB0A9.CPOSTO-SERVC
               FROM    DB2PRD.PDIDO_ESTRN_PSTAL
               WHERE   CAGPTO_CTA         = :GFCTB0A9.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A9.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A9.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A9.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_PSTAL'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0009'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A9'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

           MOVE CDEPDC       OF GFCTB0A9
                                       TO GFCTGM-AGENCIA(IND).
           MOVE CPOSTO-SERVC OF GFCTB0A9
                                       TO GFCTGM-POSTO(IND).

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTGM-DESC-NOME(IND)
           ELSE
               PERFORM 2331410-OBTER-DESC-POSTAL

               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTGM-DESC-NOME(IND)
           END-IF.

      *----------------------------------------------------------------*
       2331400-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331410-OBTER-DESC-POSTAL       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTGL-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTGL-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTGL-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC       OF GFCTB0A9
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB0A9
                                       TO GFCTGB-COD-POSTO.

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTGM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0386'         TO GFCT0M-TRANSACAO

               PERFORM 3000000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGM-FIM
               MOVE GFCTGC-ERRO        TO GFCTGM-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTGM-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTGM-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTGM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGM-DESC-MSG-ERRO
               END-IF

               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2331410-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331500-OBTER-CLIENTE           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA   OF GFCTB0A4.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0A4.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB0A4.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                       OF GFCTB0A4.

      *================================================================*
      * ST2506 - ALTERACAO PARA CAMPOS ALFANUMERICOS DE CNPJ/CPF         *
      *================================================================*
           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CCGC_CPF,
                       CFLIAL_CGC,
                       CCTRL_CPF_CGC,
ST25X6                 CCGC_CPF_ST,
ST25X6                 CFLIAL_CGC_ST,
ST25X6                 CCTRL_CPF_CGC_ST
               INTO    :GFCTB0A4.CAGPTO-CTA,
                       :GFCTB0A4.CSERVC-TARIF,
                       :GFCTB0A4.DOCOR-EVNTO,
                       :GFCTB0A4.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A4.CCGC-CPF,
                       :GFCTB0A4.CFLIAL-CGC,
                       :GFCTB0A4.CCTRL-CPF-CGC,
ST25X6                 :GFCTB0A4.CCGC-CPF-ST,
ST25X6                 :GFCTB0A4.CFLIAL-CGC-ST,
ST25X6                 :GFCTB0A4.CCTRL-CPF-CGC-ST
               FROM    DB2PRD.PDIDO_ESTRN_CLI
               WHERE   CAGPTO_CTA         = :GFCTB0A4.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A4.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A4.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A4.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A4'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

ST25X6*    MOVE CCGC-CPF      OF GFCTB0A4
ST25X6*                                TO GFCTGM-CPF-CNPJ(IND).
ST25X6*    MOVE CFLIAL-CGC    OF GFCTB0A4
ST25X6*                                TO GFCTGM-FILIAL(IND).
ST25X6*    MOVE CCTRL-CPF-CGC OF GFCTB0A4
ST25X6*                                TO GFCTGM-CONTROLE(IND).
ST25X6     MOVE CCGC-CPF-ST   OF GFCTB0A4
ST25X6                                 TO GFCTGM-CPF-CNPJ(IND).
ST25X6     MOVE CFLIAL-CGC-ST OF GFCTB0A4
ST25X6                                 TO GFCTGM-FILIAL(IND).
ST25X6     MOVE CCTRL-CPF-CGC-ST OF GFCTB0A4
ST25X6                                 TO GFCTGM-CONTROLE(IND).

      *----------------------------------------------------------------*
       2331500-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331600-OBTER-SEGMENTO          SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA   OF GFCTB0B0.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0B0.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB0B0.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                       OF GFCTB0B0.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CSGMTO_GSTAO_TARIF
               INTO    :GFCTB0B0.CAGPTO-CTA,
                       :GFCTB0B0.CSERVC-TARIF,
                       :GFCTB0B0.DOCOR-EVNTO,
                       :GFCTB0B0.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0B0.CSGMTO-GSTAO-TARIF
                          :WRK-CSGMTO-GSTAO-TARIF-NULL
               FROM    DB2PRD.PDIDO_ESTRN_SGMTO
               WHERE   CAGPTO_CTA         = :GFCTB0B0.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0B0.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0B0.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0B0.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0014'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0B0'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

           IF  WRK-CSGMTO-GSTAO-TARIF-NULL
                                       LESS ZEROS
               MOVE ZEROS              TO CSGMTO-GSTAO-TARIF OF GFCTB0B0
               GO                      TO 2331600-99-FIM
           END-IF.

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0B0
                                       TO GFCTGM-SEGMENTO(IND).

           PERFORM 2331610-OBTER-DESC-SEGMENTO.

           MOVE ISGMTO-GSTAO-TARIF OF GFCTB0H5
                                       TO GFCTGM-DESC-NOME(IND).

      *----------------------------------------------------------------*
       2331600-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2331610-OBTER-DESC-SEGMENTO     SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0B0
                                       TO CSGMTO-GSTAO-TARIF
                                                           OF GFCTB0H5.

           EXEC SQL
               SELECT  ISGMTO_GSTAO_TARIF
               INTO    :GFCTB0H5.ISGMTO-GSTAO-TARIF
               FROM    DB2PRD.TTPO_SGMTO_GSTAO
               WHERE   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0015'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0H5'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2331610-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2340000-FECHAR-CSR1-GFCTB0A7    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR1-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0016'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2340000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2350000-ABRIR-CSR2-GFCTB0A7     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGL-PONT-AGPTO       NOT EQUAL ZEROS
               MOVE GFCTGL-PONT-AGPTO  TO CAGPTO-CTA       OF GFCTB0A7
           ELSE
               MOVE GFCTGL-FILTRO-AGPTO
                                       TO CAGPTO-CTA       OF GFCTB0A7
           END-IF.

           IF  GFCTGL-PONT-TARIFA      NOT EQUAL ZEROS
               MOVE GFCTGL-PONT-TARIFA TO CSERVC-TARIF     OF GFCTB0A7
           ELSE
               MOVE GFCTGL-FILTRO-TARIFA
                                       TO CSERVC-TARIF     OF GFCTB0A7
           END-IF.

           IF  GFCTGL-PONT-DATA        EQUAL SPACES OR LOW-VALUES
               MOVE '01.01.0001'       TO DOCOR-EVNTO      OF GFCTB0A7
           ELSE
               MOVE GFCTGL-PONT-DATA   TO DOCOR-EVNTO      OF GFCTB0A7
           END-IF.

           IF  GFCTGL-PONT-TIMESTAMP   EQUAL SPACES OR LOW-VALUES
               MOVE '0001-01-01-01.01.01.000001'
                                       TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A7
           ELSE
               MOVE GFCTGL-PONT-TIMESTAMP
                                       TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A7
           END-IF.

           EXEC SQL
               OPEN CSR2-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0017'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2350000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2360000-LER-CSR2-GFCTB0A7       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR2-GFCTB0A7
               INTO    :GFCTB0A7.CAGPTO-CTA,
                       :GFCTB0A7.CSERVC-TARIF,
                       :GFCTB0A7.DOCOR-EVNTO,
                       :GFCTB0A7.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A7.CSIT-SOLTC-ESTRN,
                       :GFCTB0A7.CSIT-PROCM-ESTRN,
                       :GFCTB0A7.CINDCD-AGPTO-TOT,
                       :GFCTB0A7.CDEPDC-SOLTC-ESTRN,
                       :GFCTB0A7.CFUNC-SOLTC-ESTRN,
                       :GFCTB0A7.HSOLTC-ESTRN,
                       :GFCTB0A7.CTPO-OPER-MOTVO,
                       :GFCTB0A7.CMOTVO-JUSTF,
                       :GFCTB0A7.RJUSTF-SOLTC-ESTRN,
                       :GFCTB0A7.CDEPDC-PAREC-ESTRN,
                       :GFCTB0A7.CFUNC-PAREC-ESTRN,
                       :GFCTB0A7.HPAREC-ESTRN,
                       :GFCTB0A7.RJUSTF-PAREC-ESTRN,
                       :GFCTB0A7.QESTRN-AGPTO,
                       :GFCTB0A7.VESTRN-SOLTD,
                       :GFCTB0A7.QCANCT-AGPTO,
                       :GFCTB0A7.VCANCT-SOLTD,
                       :GFCTB0A7.DEFETV-CREDT-ESTRN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0018'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTGM-FIM
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTGM-ERRO
                   MOVE ZEROS          TO GFCTGM-COD-SQL-ERRO
                   MOVE 0484           TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 1100000-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGM-DESC-MSG-ERRO
                   PERFORM 3000000-FINALIZAR
               END-IF
               GO                      TO 2360000-99-FIM
           END-IF.

           IF  CSERVC-TARIF OF GFCTB0A7
                                       NOT EQUAL WRK-TARIFA
               PERFORM 2321000-ACESSAR-GFCTB0D8
               MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO WRK-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       2360000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2370000-FECHAR-CSR2-GFCTB0A7    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR2-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGM-COD-SQL-ERRO
               MOVE '0019'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTGM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 2210000-MOVER-ERRO-SQL
               PERFORM 3000000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2370000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000000-FINALIZAR               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
