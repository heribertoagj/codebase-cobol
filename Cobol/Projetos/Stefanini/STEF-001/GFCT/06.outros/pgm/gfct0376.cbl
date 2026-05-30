      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0376.
       AUTHOR. GIOVANE LUIZ GHIZONI.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0376                                     *
      *    PROGRAMADOR  : GIOVANE LUIZ GHIZONI        - CPM/FPOLIS     *
      *    ANALISTA CPM : SILVANA F. SOUZA MACHADO    - CPM/FPOLIS     *
      *    ANALISTA     : MARCELO         - PROCWORK  - GRUPO 50       *
      *    DATA         : 27/10/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CONSULTA  OU  EFETIVA  DEFERIMENTO  DE  ESTORNO  POR      *
      *      AGRUPAMENTO.                                              *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.PDIDO_ESTRN_GRP                  GFCTB0A7          *
      *      DB2PRD.PARM_DATA_PROCM                  GFCTB0A1          *
      *      DB2PRD.TPO_AGPTO_CTA                    GFCTB0F3          *
      *      DB2PRD.TPO_OPER_MOTVO                   GFCTB0G0          *
      *      DB2PRD.MOTVO_JUSTF_OPER                 GFCTB089          *
      *      DB2PRD.SERVC_TARIF_PRINC                GFCTB0D8          *
      *      DB2PRD.PDIDO_ESTRN_PAB                  GFCTB0A8          *
      *      DB2PRD.PDIDO_ESTRN_AG                   GFCTB0A3          *
      *      DB2PRD.PDIDO_ESTRN_EMPR                 GFCTB0A6          *
      *      DB2PRD.PDIDO_ESTRN_PSTAL                GFCTB0A9          *
      *      DB2PRD.PDIDO_ESTRN_CLI                  GFCTB0A4          *
      *      DB2PRD.PDIDO_ESTRN_SGMTO                GFCTB0B0          *
      *      DB2PRD.TTPO_SGMTO_GSTAO                 GFCTB0H5          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5500 - OBTER NOME DO CLIENTE                          *
      *      GFCT5006 - OBTER DESCRICAO DA AGENCIA                     *
      *      GFCT5060 - OBTER NOME DO CLIENTE                          *
      *      GFCT5090 - OBTER DESCRICAO DO CORRESPONDENTE BANCARIO     *
      *      GFCT5109 - OBTER DESCRICAO DO PAB                         *
      *      GFCT5110 - OBTER NOME DO CLIENTE POR CPF/CNPJ             *
      *      SENH0315 - OBTER DEPENDENCIA                              *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *                                                                *
      *    NAVEGACAO:                                                  *
      *      CHAMADO POR: GFCT7381                                     *
      *                                                                *
      *================================================================*

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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT0376  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*          AUXILIARES          *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-CONTA                   PIC X(01)           VALUE SPACES.

       01  WRK-MSG-TAB.
           03  FILLER                  PIC X(03)           VALUE ' - '.
           03  WRK-NOME-TAB            PIC X(08)           VALUE SPACES.

       01  WRK-COD-AGENCIA             PIC 9(05)           VALUE ZEROS.

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
           'ERRO NA CHAMADA DO MODULO '.
           03  WRK-MSG05-MODULO        PIC X(08)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   INDICADORES DE NULIDADE    *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB0B0-NULL.
            05 WRK-CSGMTO-GSTAO-TARIF-NULL
                                       PIC S9(04) COMP     VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DO MODULO 5500     *'.
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
           '*    AREA DO MODULO GFCT5060   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTBR'.
       COPY 'I#GFCTBS'.

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
           '*    AREA DO MODULO GFCT9110   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIU'.
       COPY 'I#GFCTIV'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5000   *'.
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
           '*    AREA DO MODULO SENH0315   *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC X(06)           VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MSG-0315        PIC 9(03)           VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC X(01)           VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC X(79)           VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC X(07)           VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC X(40)           VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*    03  WRK-CNPJ-PRI-0315       PIC 9(09)           VALUE ZEROS.
ST25X6*    03  WRK-CNPJ-FIL-0315       PIC 9(05)           VALUE ZEROS.
ST25X6     03  WRK-CNPJ-PRI-0315       PIC X(09)           VALUE SPACES.
ST25X6     03  WRK-CNPJ-FIL-0315       PIC X(04)           VALUE SPACES.
           03  WRK-CNPJ-CON-0315       PIC 9(02)           VALUE ZEROS.
       01  WRK-COD-JUNCAO-0315         PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-SECAO-0315              PIC 9(03)           VALUE ZEROS.
       01  WRK-STATUS-0315             PIC X(01)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A7
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0F3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0G0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB089
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A8
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
                INCLUDE GFCTB0A9
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0H5
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT0376   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGJ'.
       COPY 'I#GFCTGK'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTGJ-ENTRADA
                                                      GFCTGK-SAIDA
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

           PERFORM 11000-MONTAR-AREA-SAIDA.

           PERFORM 12000-VERIFICAR-ONLINE.

           PERFORM 13000-CONSISTIR-DADOS.

           PERFORM 14000-OBTER-DATA-SISTEMA.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-MONTAR-AREA-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGK-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGK-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 1010                   TO GFCTGK-LL.
           MOVE GFCTGJ-TRANSACAO       TO GFCTGK-TRANSACAO.
           MOVE GFCTGJ-FUNCAO          TO GFCTGK-FUNCAO.
           MOVE GFCTGJ-FUNC-BDSCO      TO GFCTGK-FUNC-BDSCO.
           MOVE GFCTGJ-TERMINAL        TO GFCTGK-TERMINAL.
           MOVE 'S'                    TO GFCTGK-FIM.
           MOVE ZEROS                  TO GFCTGK-ERRO
                                          GFCTGK-COD-SQL-ERRO
                                          GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           PERFORM 11100-OBTER-DESC-MENSAGEM.

           MOVE WRK-5000-DESC-MSG      TO GFCTGK-DESC-MSG-ERRO.

           MOVE GFCTGJ-OPCAO           TO GFCTGK-OPCAO.
           MOVE GFCTGJ-CAGPTO          TO GFCTGK-CAGPTO.
           MOVE GFCTGJ-CSERVC-TARIF    TO GFCTGK-CSERVC-TARIF.
           MOVE GFCTGJ-HORA            TO GFCTGK-HORA.
           MOVE GFCTGJ-DATA-EVENTO     TO GFCTGK-DATA-EVENTO.
           MOVE GFCTGJ-CGC-CPF         TO GFCTGK-CGC-CPF.
           MOVE GFCTGJ-FILIAL          TO GFCTGK-FILIAL.

           IF  GFCTGJ-CONTROLE         NOT NUMERIC
               MOVE ZEROS              TO GFCTGJ-CONTROLE
                                          GFCTGK-CONTROLE
           ELSE
               MOVE GFCTGJ-CONTROLE    TO GFCTGK-CONTROLE
           END-IF.

           MOVE GFCTGJ-CAGENCIA        TO GFCTGK-CAGENCIA.
           MOVE GFCTGJ-RAGENCIA        TO GFCTGK-RAGENCIA.
           MOVE GFCTGJ-CCONTA          TO GFCTGK-CCONTA.
           MOVE GFCTGJ-RCONTA          TO GFCTGK-RCONTA.
           MOVE GFCTGJ-CPAB            TO GFCTGK-CPAB.
           MOVE GFCTGJ-RPAB            TO GFCTGK-RPAB.
           MOVE GFCTGJ-CSEGMENTO       TO GFCTGK-CSEGMENTO.
           MOVE GFCTGJ-RSEGMENTO       TO GFCTGK-RSEGMENTO.
           MOVE GFCTGJ-DATA-SOLICITACAO
                                       TO GFCTGK-DATA-SOLICITACAO.
           MOVE GFCTGJ-ESTORNO-TOTAL   TO GFCTGK-ESTORNO-TOTAL.
           MOVE GFCTGJ-COPERACAO       TO GFCTGK-COPERACAO.
           MOVE GFCTGJ-ROPERACAO       TO GFCTGK-ROPERACAO.
           MOVE GFCTGJ-CMOTIVO         TO GFCTGK-CMOTIVO.
           MOVE GFCTGJ-RMOTIVO         TO GFCTGK-RMOTIVO.
           MOVE GFCTGJ-COMENTARIO      TO GFCTGK-COMENTARIO.
           MOVE GFCTGJ-QESTORNOS       TO GFCTGK-QESTORNOS.
           MOVE GFCTGJ-QCANCELAMENTOS  TO GFCTGK-QCANCELAMENTOS.
           MOVE GFCTGJ-VAESTORNAR      TO GFCTGK-VAESTORNAR.
           MOVE GFCTGJ-VACANCELAR      TO GFCTGK-VACANCELAR.
           MOVE GFCTGJ-PARECER         TO GFCTGK-PARECER.
           MOVE GFCTGJ-JUST            TO GFCTGK-JUST.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-DESC-MENSAGEM       SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCTGK-ERRO
                   MOVE ZEROS          TO GFCTGK-COD-SQL-ERRO
                                          GFCTGK-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGK-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTGK-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTGK-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTGK-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGK-DESC-MSG-ERRO
               END-IF
               INITIALIZE              GFCTGK-DADOS-RST
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-VERIFICAR-ONLINE          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5500-AREA-SAIDA.

           MOVE 'GFCT5500'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5500-AREA-SAIDA.

           IF  WRK-5500-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 04
                   MOVE 1              TO GFCTGK-ERRO
                   MOVE ZEROS          TO GFCTGK-COD-SQL-ERRO
                   MOVE 0030           TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGK-DESC-MSG-ERRO
                   INITIALIZE              GFCTGK-DADOS-RST
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   MOVE SPACES         TO GFCT0M-AREA-ERROS
                   INITIALIZE          GFCT0M-AREA-ERROS
                   MOVE 'GFCT0376'     TO GFCT0M-PROGRAMA
                   MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'PARM_CTRL_GSTAO'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCT0M-SQLCA-AREA
                                          GFCTGK-COD-SQL-ERRO
                   MOVE '0010'         TO GFCT0M-LOCAL
                   MOVE 9              TO GFCTGK-ERRO
                   MOVE 0012           TO GFCTGK-COD-MSG-ERRO
                   MOVE WRK-5500-DESC-MSG
                                       TO GFCTGK-DESC-MSG-ERRO
                   INITIALIZE          GFCTGK-DADOS-RST
                   PERFORM 30000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTGJ-TRANSACAO        EQUAL SPACES)       OR
              (GFCTGJ-FUNCAO           EQUAL SPACES)       OR
              (GFCTGJ-FUNC-BDSCO       EQUAL SPACES)       OR
              (GFCTGJ-OPCAO            NOT NUMERIC)        OR
              (GFCTGJ-OPCAO            NOT EQUAL 1 AND 2)  OR
              (GFCTGJ-CAGPTO           NOT NUMERIC)        OR
              (GFCTGJ-CAGPTO           NOT EQUAL 4 AND 14
                                            AND  9 AND 15
                                            AND 16 AND 17)
               MOVE 0001               TO GFCTGK-ERRO
                                          GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO GFCTGK-COD-SQL-ERRO
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTGK-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       13000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       14000-OBTER-DATA-SISTEMA        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
                SELECT CSIST_PRINC,
                       DPROCM_ANTER,
                       DPROCM_ATUAL,
                       DPROCM_PROX
                INTO   :GFCTB0A1.CSIST-PRINC,
                       :GFCTB0A1.DPROCM-ANTER,
                       :GFCTB0A1.DPROCM-ATUAL,
                       :GFCTB0A1.DPROCM-PROX
                FROM   DB2PRD.PARM_DATA_PROCM
                WHERE  CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       14000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       14100-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0376'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 9                      TO GFCTGK-ERRO.

           PERFORM 11100-OBTER-DESC-MENSAGEM.

           STRING WRK-5000-DESC-MSG
                  WRK-MSG-TAB
           DELIMITED BY '  '           INTO GFCTGK-DESC-MSG-ERRO.

           INITIALIZE              GFCTGK-DADOS-RST.

           PERFORM 30000-FINALIZAR.

      *----------------------------------------------------------------*
       14100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGJ-OPCAO            EQUAL 1
               PERFORM 21000-CONSULTAR
               PERFORM 22000-CARREGAR-SAIDA
               PERFORM 23000-OBTER-DADOS-CAGPTO
           ELSE
               PERFORM 24000-EFETIVAR-DEFERIMENTO
           END-IF.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-CONSULTAR                 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGJ-CAGPTO          TO CAGPTO-CTA   OF GFCTB0A7.
           MOVE GFCTGJ-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0A7.
           MOVE GFCTGJ-DATA-EVENTO     TO DOCOR-EVNTO  OF GFCTB0A7.
           MOVE GFCTGJ-HORA            TO HIDTFD-ESTRN-AGPTO
                                          OF GFCTB0A7.

           EXEC SQL
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
                INTO   :GFCTB0A7.CAGPTO-CTA,
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
                FROM   DB2PRD.PDIDO_ESTRN_GRP
                WHERE  CAGPTO_CTA      = :GFCTB0A7.CAGPTO-CTA   AND
                       CSERVC_TARIF    = :GFCTB0A7.CSERVC-TARIF AND
                       DOCOR_EVNTO     = :GFCTB0A7.DOCOR-EVNTO  AND
                       HIDTFD_ESTRN_AGPTO
                                       = :GFCTB0A7.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-CARREGAR-SAIDA            SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0A7 TO GFCTGK-CAGPTO.
           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO GFCTGK-CSERVC-TARIF.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO GFCTGK-HORA.
           MOVE DOCOR-EVNTO OF GFCTB0A7
                                       TO GFCTGK-DATA-EVENTO.
           MOVE CINDCD-AGPTO-TOT OF GFCTB0A7
                                       TO GFCTGK-ESTORNO-TOTAL.

           STRING HIDTFD-ESTRN-AGPTO OF GFCTB0A7(9:2) '.'
                  HIDTFD-ESTRN-AGPTO OF GFCTB0A7(6:2) '.'
                  HIDTFD-ESTRN-AGPTO OF GFCTB0A7(1:4) '.'
           DELIMITED BY SIZE           INTO GFCTGK-DATA-SOLICITACAO.

           MOVE QESTRN-AGPTO OF GFCTB0A7
                                       TO GFCTGK-QESTORNOS.
           MOVE QCANCT-AGPTO OF GFCTB0A7
                                       TO GFCTGK-QCANCELAMENTOS.
           MOVE VESTRN-SOLTD OF GFCTB0A7
                                       TO GFCTGK-VAESTORNAR.
           MOVE VCANCT-SOLTD OF GFCTB0A7
                                       TO GFCTGK-VACANCELAR.
           MOVE RJUSTF-SOLTC-ESTRN-TEXT OF GFCTB0A7
                                       TO GFCTGK-COMENTARIO.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-OBTER-DADOS-CAGPTO        SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTGJ-CAGPTO
               WHEN 15
                   PERFORM 23100-OBTER-POSTO
               WHEN 14
                   PERFORM 23200-OBTER-AGENCIA
               WHEN 16
                   PERFORM 23300-OBTER-EMPRESA
               WHEN 17
                   PERFORM 23400-OBTER-POSTAL
               WHEN 4
                   PERFORM 23500-OBTER-CLIENTE
               WHEN OTHER
                   PERFORM 23600-OBTER-SEGMENTO
           END-EVALUATE.

           MOVE CTPO-OPER-MOTVO OF GFCTB0A7
                                       TO GFCTGK-COPERACAO.

           PERFORM 23700-OBTER-DESC-COD-OPER.

           MOVE RTPO-OPER-MOTVO OF GFCTB0G0
                                       TO GFCTGK-ROPERACAO.

           MOVE CMOTVO-JUSTF OF GFCTB0A7
                                       TO GFCTGK-CMOTIVO.

           PERFORM 23800-OBTER-DESC-MOT-OPER.

           MOVE RMOTVO-JUSTF OF GFCTB089
                                       TO GFCTGK-RMOTIVO.

      *----------------------------------------------------------------*
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23100-OBTER-POSTO               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGJ-CAGPTO          TO CAGPTO-CTA   OF GFCTB0A8.
           MOVE GFCTGJ-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0A8.
           MOVE GFCTGJ-DATA-EVENTO     TO DOCOR-EVNTO  OF GFCTB0A8.
           MOVE GFCTGJ-HORA            TO HIDTFD-ESTRN-AGPTO
                                          OF GFCTB0A8.

           EXEC SQL
                SELECT CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
                INTO  :GFCTB0A8.CAGPTO-CTA,
                      :GFCTB0A8.CSERVC-TARIF,
                      :GFCTB0A8.DOCOR-EVNTO,
                      :GFCTB0A8.HIDTFD-ESTRN-AGPTO,
                      :GFCTB0A8.CEMPR-INC,
                      :GFCTB0A8.CDEPDC,
                      :GFCTB0A8.CPOSTO-SERVC
                FROM  DB2PRD.PDIDO_ESTRN_PAB
                WHERE CAGPTO_CTA       = :GFCTB0A8.CAGPTO-CTA   AND
                      CSERVC_TARIF     = :GFCTB0A8.CSERVC-TARIF AND
                      DOCOR_EVNTO      = :GFCTB0A8.DOCOR-EVNTO  AND
                      HIDTFD_ESTRN_AGPTO
                                       = :GFCTB0A8.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_PAB'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A8'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           MOVE CDEPDC OF GFCTB0A8     TO WRK-COD-AGENCIA.

           PERFORM 23210-OBTER-DESC-AGENCIA.

           MOVE GFCT1X-DESC-DEPDC(1)   TO GFCTGK-RAGENCIA.

           MOVE CDEPDC OF GFCTB0A8     TO GFCTGK-CAGENCIA

           MOVE CPOSTO-SERVC OF GFCTB0A8
                                       TO GFCTGK-CPAB.

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTGK-RPAB
           ELSE
               PERFORM 23110-OBTER-DESC-PAB

               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTGK-RPAB
           END-IF.

      *----------------------------------------------------------------*
       23100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23110-OBTER-DESC-PAB            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTGJ-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTGJ-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTGJ-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
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
               MOVE 9                  TO GFCTGK-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0378'         TO GFCT0M-TRANSACAO

               PERFORM 30000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGK-FIM
               MOVE GFCTGC-ERRO        TO GFCTGK-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTGK-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTGK-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTGK-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGK-DESC-MSG-ERRO
               END-IF

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23200-OBTER-AGENCIA             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGJ-CAGPTO          TO CAGPTO-CTA   OF GFCTB0A3.
           MOVE GFCTGJ-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0A3.
           MOVE GFCTGJ-DATA-EVENTO     TO DOCOR-EVNTO  OF GFCTB0A3.
           MOVE GFCTGJ-HORA            TO HIDTFD-ESTRN-AGPTO
                                          OF GFCTB0A3.

           EXEC SQL
                SELECT CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CEMPR_INC,
                       CDEPDC
                INTO   :GFCTB0A3.CAGPTO-CTA,
                       :GFCTB0A3.CSERVC-TARIF,
                       :GFCTB0A3.DOCOR-EVNTO,
                       :GFCTB0A3.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A3.CEMPR-INC,
                       :GFCTB0A3.CDEPDC
                FROM   DB2PRD.PDIDO_ESTRN_AG
                WHERE  CAGPTO_CTA      = :GFCTB0A3.CAGPTO-CTA   AND
                       CSERVC_TARIF    = :GFCTB0A3.CSERVC-TARIF AND
                       DOCOR_EVNTO     = :GFCTB0A3.DOCOR-EVNTO  AND
                       HIDTFD_ESTRN_AGPTO
                                       = :GFCTB0A3.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_AG'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A3'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           MOVE CDEPDC OF GFCTB0A3     TO GFCTGK-CAGENCIA.

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODAS'            TO GFCTGK-RAGENCIA
           ELSE
               MOVE CDEPDC OF GFCTB0A3 TO WRK-COD-AGENCIA

               PERFORM 23210-OBTER-DESC-AGENCIA

               MOVE GFCT1X-DESC-DEPDC(1)
                                       TO GFCTGK-RAGENCIA
           END-IF.

      *----------------------------------------------------------------*
       23200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23210-OBTER-DESC-AGENCIA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.

           MOVE GFCTGJ-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTGJ-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTGJ-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE WRK-COD-AGENCIA        TO GFCT1V-COD-DEPDC(1).

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
               MOVE 9                  TO GFCTGK-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0378'         TO GFCT0M-TRANSACAO

               PERFORM 30000-FINALIZAR
           END-IF

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGK-FIM
               MOVE GFCT1X-ERRO        TO GFCTGK-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTGK-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTGK-COD-MSG-ERRO

               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTGK-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'
                          GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGK-DESC-MSG-ERRO
               END-IF

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23210-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23300-OBTER-EMPRESA             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGJ-CAGPTO          TO CAGPTO-CTA   OF GFCTB0A6.
           MOVE GFCTGJ-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0A6.
           MOVE GFCTGJ-DATA-EVENTO     TO DOCOR-EVNTO  OF GFCTB0A6.
           MOVE GFCTGJ-HORA            TO HIDTFD-ESTRN-AGPTO
                                          OF GFCTB0A6.

           EXEC SQL
                SELECT CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CAG_EMPR,
                       CRZ_CTA_EMPR,
                       CNRO_CTA_DEB,
                       CLCTO_CTA_EMPR
                INTO   :GFCTB0A6.CAGPTO-CTA,
                       :GFCTB0A6.CSERVC-TARIF,
                       :GFCTB0A6.DOCOR-EVNTO,
                       :GFCTB0A6.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A6.CAG-EMPR,
                       :GFCTB0A6.CRZ-CTA-EMPR,
                       :GFCTB0A6.CNRO-CTA-DEB,
                       :GFCTB0A6.CLCTO-CTA-EMPR
                FROM   DB2PRD.PDIDO_ESTRN_EMPR
                WHERE  CAGPTO_CTA      = :GFCTB0A6.CAGPTO-CTA    AND
                       CSERVC_TARIF    = :GFCTB0A6.CSERVC-TARIF  AND
                       DOCOR_EVNTO     = :GFCTB0A6.DOCOR-EVNTO   AND
                       HIDTFD_ESTRN_AGPTO
                                       = :GFCTB0A6.HIDTFD-ESTRN-AGPTO
            END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_EMPR' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A6'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           MOVE CAG-EMPR OF GFCTB0A6   TO GFCTGK-CAGENCIA.
           MOVE CNRO-CTA-DEB OF GFCTB0A6
                                       TO GFCTGK-CCONTA.

           PERFORM 23310-OBTER-DESC-EMPRESA.

           MOVE GFCTBS-NOME-CLIENTE(1) TO GFCTGK-RCONTA.

      *----------------------------------------------------------------*
       23300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23310-OBTER-DESC-EMPRESA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTBR-ENTRADA.

           MOVE 100                    TO GFCTBR-LL.
           MOVE ZEROS                  TO GFCTBR-ZZ.

           MOVE GFCTGJ-TRANSACAO       TO GFCTBR-TRANSACAO.
           MOVE GFCTGJ-FUNCAO          TO GFCTBR-FUNCAO.
           MOVE ZEROS                  TO GFCTBR-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTBR-QTDE-TOT-REG.
           MOVE GFCTGJ-FUNC-BDSCO      TO GFCTBR-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTBR-FIM.

           MOVE CAG-EMPR OF GFCTB0A6   TO GFCTBR-COD-DEPDC.
           MOVE CNRO-CTA-DEB OF GFCTB0A6
                                       TO GFCTBR-CCTA-CLI.

           MOVE SPACES                 TO GFCTBS-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTBS-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5060'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTBR-ENTRADA
                                             GFCTBS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTGK-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0376'         TO GFCT0M-TRANSACAO

               PERFORM 30000-FINALIZAR
           END-IF

           IF  GFCTBS-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGK-FIM
               MOVE GFCTBS-ERRO        TO GFCTGK-ERRO
               MOVE GFCTBS-COD-SQL-ERRO
                                       TO GFCTGK-COD-SQL-ERRO
               MOVE GFCTBS-COD-MSG-ERRO
                                       TO GFCTGK-COD-MSG-ERRO

               IF  GFCTBS-ERRO         EQUAL 1
                   MOVE GFCTBS-DESC-MSG-ERRO
                                       TO GFCTGK-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5060-'
                          GFCTBS-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGK-DESC-MSG-ERRO
               END-IF

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23400-OBTER-POSTAL              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGJ-CAGPTO          TO CAGPTO-CTA   OF GFCTB0A9.
           MOVE GFCTGJ-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0A9.
           MOVE GFCTGJ-DATA-EVENTO     TO DOCOR-EVNTO  OF GFCTB0A9.
           MOVE GFCTGJ-HORA            TO HIDTFD-ESTRN-AGPTO
                                          OF GFCTB0A9.

           EXEC SQL
                SELECT CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
                INTO   :GFCTB0A9.CAGPTO-CTA,
                       :GFCTB0A9.CSERVC-TARIF,
                       :GFCTB0A9.DOCOR-EVNTO,
                       :GFCTB0A9.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A9.CEMPR-INC,
                       :GFCTB0A9.CDEPDC,
                       :GFCTB0A9.CPOSTO-SERVC
                FROM   DB2PRD.PDIDO_ESTRN_PSTAL
                WHERE  CAGPTO_CTA      = :GFCTB0A9.CAGPTO-CTA   AND
                       CSERVC_TARIF    = :GFCTB0A9.CSERVC-TARIF AND
                       DOCOR_EVNTO     = :GFCTB0A9.DOCOR-EVNTO  AND
                       HIDTFD_ESTRN_AGPTO
                                       = :GFCTB0A9.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_PSTAL'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A9'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           MOVE CDEPDC OF GFCTB0A9     TO WRK-COD-AGENCIA.

           PERFORM 23210-OBTER-DESC-AGENCIA.

           MOVE GFCT1X-DESC-DEPDC(1)   TO GFCTGK-RAGENCIA.

           MOVE CDEPDC OF GFCTB0A9     TO GFCTGK-CAGENCIA.

           MOVE CPOSTO-SERVC OF GFCTB0A9
                                       TO GFCTGK-CPAB.

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTGK-RPAB
           ELSE
               PERFORM 23410-OBTER-DESC-POSTAL

               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTGK-RPAB
           END-IF.

      *----------------------------------------------------------------*
       23400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23410-OBTER-DESC-POSTAL         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTGJ-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTGJ-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTGJ-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
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
               MOVE 9                  TO GFCTGK-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0376'         TO GFCT0M-TRANSACAO

               PERFORM 30000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGK-FIM
               MOVE GFCTGC-ERRO        TO GFCTGK-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTGK-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTGK-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTGK-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGK-DESC-MSG-ERRO
               END-IF

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23410-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23500-OBTER-CLIENTE             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGJ-CAGPTO          TO CAGPTO-CTA   OF GFCTB0A4.
           MOVE GFCTGJ-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0A4.
           MOVE GFCTGJ-DATA-EVENTO     TO DOCOR-EVNTO  OF GFCTB0A4.
           MOVE GFCTGJ-HORA            TO HIDTFD-ESTRN-AGPTO
                                          OF GFCTB0A4.

           EXEC SQL
                SELECT CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
ST2506                 CCGC_CPF,
ST2506                 CFLIAL_CGC,
ST2506                 CCTRL_CPF_CGC
ST2506                 CCGC_CPF_ST,
ST2506                 CFLIAL_CGC_ST,
ST2506                 CCTRL_CPF_CGC_ST
                INTO   :GFCTB0A4.CAGPTO-CTA,
                       :GFCTB0A4.CSERVC-TARIF,
                       :GFCTB0A4.DOCOR-EVNTO,
                       :GFCTB0A4.HIDTFD-ESTRN-AGPTO,
ST2506                 :GFCTB0A4.CCGC-CPF,
ST2506                 :GFCTB0A4.CFLIAL-CGC,
ST2506                 :GFCTB0A4.CCTRL-CPF-CGC
ST2506                 :GFCTB0A4.CCGC-CPF-ST,
ST2506                 :GFCTB0A4.CFLIAL-CGC-ST,
ST2506                 :GFCTB0A4.CCTRL-CPF-CGC-ST
                FROM   DB2PRD.PDIDO_ESTRN_CLI
                WHERE  CAGPTO_CTA      = :GFCTB0A4.CAGPTO-CTA    AND
                       CSERVC_TARIF    = :GFCTB0A4.CSERVC-TARIF  AND
                       DOCOR_EVNTO     = :GFCTB0A4.DOCOR-EVNTO   AND
                       HIDTFD_ESTRN_AGPTO
                                       = :GFCTB0A4.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A4'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

ST2506*    MOVE CCGC-CPF OF GFCTB0A4   TO GFCTGK-CGC-CPF.
ST2506*    MOVE CFLIAL-CGC OF GFCTB0A4 TO GFCTGK-FILIAL.
ST2506*    MOVE CCTRL-CPF-CGC OF GFCTB0A4
ST2506*                                TO GFCTGK-CONTROLE.
ST2506     MOVE CCGC-CPF-ST            OF GFCTB0A4   
                                       TO GFCTGK-CGC-CPF.
ST2506     MOVE CFLIAL-CGC-ST          OF GFCTB0A4 
                                       TO GFCTGK-FILIAL.
ST2506     MOVE CCTRL-CPF-CGC-ST       OF GFCTB0A4
ST2506                                 TO GFCTGK-CONTROLE.

           PERFORM 23510-OBTER-DESC-CLIENTE.

           MOVE GFCTIV-NOME-CLIENTE    TO GFCTGK-RCONTA.

      *----------------------------------------------------------------*
       23500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23510-OBTER-DESC-CLIENTE        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIU-ENTRADA.

           INITIALIZE                  GFCTIU-ENTRADA.

           MOVE 100                    TO GFCTIU-LL.
           MOVE ZEROS                  TO GFCTIU-ZZ.

           MOVE GFCTGJ-TRANSACAO       TO GFCTIU-TRANSACAO.
           MOVE GFCTGJ-FUNCAO          TO GFCTIU-FUNCAO.
           MOVE GFCTGJ-FUNC-BDSCO      TO GFCTIU-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTIU-FIM.
ST2506*    MOVE CCGC-CPF OF GFCTB0A4   TO GFCTIU-NRO-CPF-CNPJ.
ST2506*    MOVE CFLIAL-CGC OF GFCTB0A4 TO GFCTIU-NRO-FILIAL.
ST2506*    MOVE CCTRL-CPF-CGC OF GFCTB0A4
ST2506*                                TO GFCTIU-CTRL-CPF-CNPJ.
ST2506     MOVE CCGC-CPF-ST            OF GFCTB0A4   
ST2506                                 TO GFCTIU-COD-CPF-CNPJ.
ST2506     MOVE CFLIAL-CGC-ST          OF GFCTB0A4 
ST2506                                 TO GFCTIU-COD-FILIAL.
ST2506     MOVE CCTRL-CPF-CGC-ST       OF GFCTB0A4
ST2506                                 TO GFCTIU-CTRL-CPF-CNPJ.

           MOVE SPACES                 TO GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTIV-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5110'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTIU-ENTRADA
                                             GFCTIV-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTGK-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0376'         TO GFCT0M-TRANSACAO

               PERFORM 30000-FINALIZAR
           END-IF

           IF  GFCTIV-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGK-FIM
               MOVE GFCTIV-ERRO        TO GFCTGK-ERRO
               MOVE GFCTIV-COD-SQL-ERRO
                                       TO GFCTGK-COD-SQL-ERRO
               MOVE GFCTIV-COD-MSG-ERRO
                                       TO GFCTGK-COD-MSG-ERRO

               IF  GFCTIV-ERRO         EQUAL 1
                   MOVE GFCTIV-DESC-MSG-ERRO
                                       TO GFCTGK-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5110-'
                          GFCTIV-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGK-DESC-MSG-ERRO
               END-IF

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23510-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23600-OBTER-SEGMENTO            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGJ-CAGPTO          TO CAGPTO-CTA   OF GFCTB0B0.
           MOVE GFCTGJ-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0B0.
           MOVE GFCTGJ-DATA-EVENTO     TO DOCOR-EVNTO  OF GFCTB0B0.
           MOVE GFCTGJ-HORA            TO HIDTFD-ESTRN-AGPTO
                                          OF GFCTB0B0.

           EXEC SQL
                SELECT CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CSGMTO_GSTAO_TARIF
                INTO   :GFCTB0B0.CAGPTO-CTA,
                       :GFCTB0B0.CSERVC-TARIF,
                       :GFCTB0B0.DOCOR-EVNTO,
                       :GFCTB0B0.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0B0.CSGMTO-GSTAO-TARIF
                          :WRK-CSGMTO-GSTAO-TARIF-NULL
                FROM   DB2PRD.PDIDO_ESTRN_SGMTO
                WHERE  CAGPTO_CTA      = :GFCTB0B0.CAGPTO-CTA    AND
                       CSERVC_TARIF    = :GFCTB0B0.CSERVC-TARIF  AND
                       DOCOR_EVNTO     = :GFCTB0B0.DOCOR-EVNTO   AND
                       HIDTFD_ESTRN_AGPTO
                                       = :GFCTB0B0.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0B0'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           IF  WRK-CSGMTO-GSTAO-TARIF-NULL
                                       LESS ZEROS
               GO                      TO 23600-99-FIM
           END-IF.

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0B0
                                       TO GFCTGK-CSEGMENTO

           PERFORM 23610-OBTER-DESC-SEGMENTO.

           MOVE ISGMTO-GSTAO-TARIF OF GFCTB0H5
                                       TO GFCTGK-RSEGMENTO.

      *----------------------------------------------------------------*
       23600-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23610-OBTER-DESC-SEGMENTO       SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0B0
                                       TO CSGMTO-GSTAO-TARIF
                                          OF GFCTB0H5.

           EXEC SQL
                SELECT ISGMTO_GSTAO_TARIF
                INTO   :GFCTB0H5.ISGMTO-GSTAO-TARIF
                FROM   DB2PRD.TTPO_SGMTO_GSTAO
                WHERE  CSGMTO_GSTAO_TARIF
                                       = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0H5'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       23610-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23700-OBTER-DESC-COD-OPER       SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO OF GFCTB0A7
                                       TO CTPO-OPER-MOTVO OF GFCTB0G0.

           EXEC SQL
                SELECT RTPO_OPER_MOTVO
                INTO   :GFCTB0G0.RTPO-OPER-MOTVO
                FROM   DB2PRD.TPO_OPER_MOTVO
                WHERE  CTPO_OPER_MOTVO  = :GFCTB0G0.CTPO-OPER-MOTVO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TPO_OPER_MOTVO'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0G0'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       23700-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23800-OBTER-DESC-MOT-OPER       SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO OF GFCTB0A7
                                       TO CTPO-OPER-MOTVO OF GFCTB089.
           MOVE CMOTVO-JUSTF OF GFCTB0A7
                                       TO CMOTVO-JUSTF OF GFCTB089.

           EXEC SQL
                SELECT RMOTVO_JUSTF
                INTO   :GFCTB089.RMOTVO-JUSTF
                FROM   DB2PRD.MOTVO_JUSTF_OPER
                WHERE  CTPO_OPER_MOTVO = :GFCTB089.CTPO-OPER-MOTVO   AND
                       CMOTVO_JUSTF    = :GFCTB089.CMOTVO-JUSTF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'MOVTO_JUSTF_OPER' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB089'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       23800-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24000-EFETIVAR-DEFERIMENTO      SECTION.
      *----------------------------------------------------------------*

           PERFORM 24050-REFAZER-CONSULTA.

           IF  GFCTGJ-QESTORNOS        EQUAL ZEROS AND
               GFCTGJ-QCANCELAMENTOS   EQUAL ZEROS
               IF  GFCTGJ-PARECER      EQUAL 2
                   MOVE 1              TO GFCTGK-ERRO
                   MOVE 0515           TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE ZEROS          TO GFCTGK-COD-SQL-ERRO
                   PERFORM 11100-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGK-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
           END-IF.

           MOVE GFCTGJ-CAGPTO          TO CAGPTO-CTA   OF GFCTB0A7.
           MOVE GFCTGJ-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0A7.
           MOVE GFCTGJ-DATA-EVENTO     TO DOCOR-EVNTO  OF GFCTB0A7.
           MOVE GFCTGJ-HORA            TO HIDTFD-ESTRN-AGPTO
                                          OF GFCTB0A7.
           MOVE GFCTGJ-PARECER         TO CSIT-SOLTC-ESTRN OF GFCTB0A7.

           PERFORM 24100-OBTER-DEPENDENCIA.

           MOVE WRK-COD-JUNCAO-0315    TO CDEPDC-PAREC-ESTRN
                                          OF GFCTB0A7.
           MOVE GFCTGJ-FUNC-BDSCO      TO CFUNC-PAREC-ESTRN
                                          OF GFCTB0A7.
           MOVE GFCTGJ-JUST            TO RJUSTF-PAREC-ESTRN-TEXT
                                          OF GFCTB0A7.
           MOVE LENGTH OF RJUSTF-PAREC-ESTRN-TEXT OF GFCTB0A7
                                       TO RJUSTF-PAREC-ESTRN-LEN
                                          OF GFCTB0A7.

           IF  GFCTGJ-PARECER  NOT EQUAL  2
               MOVE 1                  TO CSIT-PROCM-ESTRN OF GFCTB0A7
           END-IF.

           PERFORM 24200-ATUALIZAR-GFCTB0A7.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24050-REFAZER-CONSULTA          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGJ-CAGPTO          TO CAGPTO-CTA   OF GFCTB0A7.
           MOVE GFCTGJ-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0A7.
           MOVE GFCTGJ-DATA-EVENTO     TO DOCOR-EVNTO  OF GFCTB0A7.
           MOVE GFCTGJ-HORA            TO HIDTFD-ESTRN-AGPTO
                                          OF GFCTB0A7.

           EXEC SQL
                SELECT CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CSIT_SOLTC_ESTRN,
                       CSIT_PROCM_ESTRN,
                       CDEPDC_PAREC_ESTRN,
                       CFUNC_PAREC_ESTRN,
                       HPAREC_ESTRN,
                       RJUSTF_PAREC_ESTRN
                INTO   :GFCTB0A7.CAGPTO-CTA,
                       :GFCTB0A7.CSERVC-TARIF,
                       :GFCTB0A7.DOCOR-EVNTO,
                       :GFCTB0A7.HIDTFD-ESTRN-AGPTO,
                       :GFCTB0A7.CSIT-SOLTC-ESTRN,
                       :GFCTB0A7.CSIT-PROCM-ESTRN,
                       :GFCTB0A7.CDEPDC-PAREC-ESTRN,
                       :GFCTB0A7.CFUNC-PAREC-ESTRN,
                       :GFCTB0A7.HPAREC-ESTRN,
                       :GFCTB0A7.RJUSTF-PAREC-ESTRN
                FROM  DB2PRD.PDIDO_ESTRN_GRP
                WHERE CAGPTO_CTA       = :GFCTB0A7.CAGPTO-CTA
                 AND  CSERVC_TARIF     = :GFCTB0A7.CSERVC-TARIF
                 AND  DOCOR_EVNTO      = :GFCTB0A7.DOCOR-EVNTO
                 AND  HIDTFD_ESTRN_AGPTO
                                       = :GFCTB0A7.HIDTFD-ESTRN-AGPTO
                 AND  CSIT_SOLTC_ESTRN = 1
                 AND  CSIT_PROCM_ESTRN = 3
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 ) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           IF (SQLCODE                 EQUAL  +100 )
               MOVE 1                  TO GFCTGK-ERRO
               MOVE 0560               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO GFCTGK-COD-SQL-ERRO
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG
                                       TO GFCTGK-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24050-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24100-OBTER-DEPENDENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGJ-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.
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
               MOVE 1                  TO GFCTGK-ERRO
               MOVE 0397               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO GFCTGK-COD-SQL-ERRO
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTGK-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24200-ATUALIZAR-GFCTB0A7        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                UPDATE DB2PRD.PDIDO_ESTRN_GRP
                SET    CSIT_SOLTC_ESTRN
                                       = :GFCTB0A7.CSIT-SOLTC-ESTRN,
                       CSIT_PROCM_ESTRN
                                       = :GFCTB0A7.CSIT-PROCM-ESTRN,
                       CDEPDC_PAREC_ESTRN
                                       = :GFCTB0A7.CDEPDC-PAREC-ESTRN,
                       HPAREC_ESTRN    = CURRENT TIMESTAMP,
                       CFUNC_PAREC_ESTRN
                                       = :GFCTB0A7.CFUNC-PAREC-ESTRN,
                       RJUSTF_PAREC_ESTRN
                                       = :GFCTB0A7.RJUSTF-PAREC-ESTRN
                WHERE  CAGPTO_CTA      = :GFCTB0A7.CAGPTO-CTA   AND
                       CSERVC_TARIF    = :GFCTB0A7.CSERVC-TARIF AND
                       DOCOR_EVNTO     = :GFCTB0A7.DOCOR-EVNTO  AND
                       HIDTFD_ESTRN_AGPTO
                                       = :GFCTB0A7.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGK-COD-SQL-ERRO
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0019               TO GFCTGK-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       24200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
