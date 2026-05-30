      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0375.
       AUTHOR. FERNANDO NAIM SCHMITZ.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0375                                     *
      *    PROGRAMADOR  : FERNANDO NAIM SCHMITZ        - CPM/FPOLIS    *
      *    ANALISTA CPM : SILVANA F. SOUZA MACHADO     - CPM/FPOLIS    *
      *    ANALISTA     : MARCELO  - PROCWORK          - GRUPO 50      *
      *    DATA         : 27/10/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CONSULTA OU EFETIVA ESTORNO POR AGRUPAMENTO.              *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.PARM_DATA_PROCM                  GFCTB0A1          *
      *      DB2PRD.PDIDO_ESTRN_AG                   GFCTB0A3          *
      *      DB2PRD.PDIDO_ESTRN_CLI                  GFCTB0A4          *
      *      DB2PRD.PDIDO_ESTRN_EMPR                 GFCTB0A6          *
      *      DB2PRD.PDIDO_ESTRN_GRP                  GFCTB0A7          *
      *      DB2PRD.PDIDO_ESTRN_PAB                  GFCTB0A8          *
      *      DB2PRD.PDIDO_ESTRN_PSTAL                GFCTB0A9          *
      *      DB2PRD.PDIDO_ESTRN_SGMTO                GFCTB0B0          *
      *      DB2PRD.SERVC_TARIF_PRINC                GFCTB0D8          *
      *      DB2PRD.TPO_AGPTO_CTA                    GFCTB0F3          *
      *      DB2PRD.TPO_OPER_MOTVO                   GFCTB0G0          *
      *      DB2PRD.MOTVO_JUSTF_OPER                 GFCTB089          *
      *      DB2PRD.TTPO_SGMTO_GSTAO                 GFCTB0H5          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS.                  *
      *      GFCT5500 - VERIFICAR SE O SISTEMA ESTA LIBERADO.          *
      *      GFCT5006 - OBTER DESCRICAO DA AGENCIA                     *
      *      GFCT5060 - OBTER NOME DO CLIENTE POR AGENCIA/CONTA        *
      *      GFCT5090 - OBTER DESCRICAO DO CORRESPONDENTE BANCARIO     *
      *      GFCT5109 - OBTER DESCRICAO DO PAB                         *
      *      GFCT5110 - OBTER NOME DO CLIENTE POR CPF/CNPJ             *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                *
      *                                                                *
      *    NAVEGACAO:                                                  *
      *      CHAMADO POR: GFCT7372                                     *
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
           '*  INICIO DA WORKING GFCT0375  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*          AUXILIARES          *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.
       01  WRK-CONTA                   PIC X(01)           VALUE SPACES.

       01  WRK-COD-AGENCIA             PIC 9(05)           VALUE ZEROS.

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
           'ERRO NA CHAMADA DO MODULO '.
           03  WRK-MSG05-MODULO        PIC X(08)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS DE NULIDADE     *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB0B0-NULL.
           03  WRK-CSGMTO-GSTAO-TARIF-NULL
                                       PIC S9(04) COMP     VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE MODULO 5000     *'.
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
           '*      AREA DE MODULO 5500     *'.
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
           '*      AREA DE TABELAS DB2     *'.
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
                INCLUDE GFCTB0D8
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
                INCLUDE GFCTB0H5
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT0375   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFV'.
       COPY 'I#GFCTFW'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTFV-ENTRADA
                                                      GFCTFW-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-PROCESSAR-PROGRAMA.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           PERFORM 110000-MONTAR-AREA-SAIDA.

           PERFORM 120000-VERIFICAR-ONLINE.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-MONTAR-AREA-SAIDA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTFW-SAIDA.

           INITIALIZE                  GFCTFW-SAIDA.

           MOVE ZEROS                  TO GFCTFW-CONTROLE.

           MOVE 1010                   TO GFCTFW-LL.
           MOVE GFCTFV-TRANSACAO       TO GFCTFW-TRANSACAO.
           MOVE GFCTFV-FUNC-BDSCO      TO GFCTFW-FUNC-BDSCO.
           MOVE GFCTFV-TERMINAL        TO GFCTFW-TERMINAL.
           MOVE GFCTFV-FUNCAO          TO GFCTFW-FUNCAO.
           MOVE ZEROS                  TO GFCTFW-ERRO
                                          GFCTFW-COD-SQL-ERRO
                                          GFCTFW-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           MOVE 'S'                    TO GFCTFW-FIM.

           PERFORM 111000-OBTER-DESC-MENSAGEM.

           MOVE WRK-5000-DESC-MSG      TO GFCTFW-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-OBTER-DESC-MENSAGEM      SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE ZEROS          TO GFCTFW-COD-SQL-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTFW-COD-SQL-ERRO
               END-IF
               MOVE ZEROS              TO GFCTFW-COD-MSG-ERRO
               MOVE 1                  TO GFCTFW-ERRO
               MOVE WRK-5000-DESC-MSG  TO GFCTFW-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-VERIFICAR-ONLINE         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5500-AREA-SAIDA.

           MOVE 'GFCT5500'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5500-AREA-SAIDA.

           IF  WRK-5500-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 04
                   MOVE 1              TO GFCTFW-ERRO
                   MOVE ZEROS          TO GFCTFW-COD-SQL-ERRO
                   MOVE 0030           TO GFCTFW-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFW-DESC-MSG-ERRO
               END-IF
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   MOVE 'GFCT0375'     TO GFCT0M-PROGRAMA
                   MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'PARM_CTRL_GSTAO'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCT0M-SQLCA-AREA
                                          GFCTFW-COD-SQL-ERRO
                   MOVE 9              TO GFCTFW-ERRO
                   MOVE 0012           TO GFCTFW-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE '0010'         TO GFCT0M-LOCAL
                   MOVE WRK-5500-DESC-MSG
                                       TO GFCTFW-DESC-MSG-ERRO
               END-IF
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-CONSISTIR-DADOS.

           PERFORM 220000-OBTER-DATA-SIST.

           IF  GFCTFV-OPCAO            EQUAL 1
               PERFORM 230000-CONSULTAR-GFCTB0A7
               PERFORM 240000-MOVER-CAMPOS
           ELSE
               PERFORM 250000-EFETIVAR
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           IF (GFCTFV-TRANSACAO        EQUAL SPACES) OR
              (GFCTFV-FUNCAO           EQUAL SPACES) OR
              (GFCTFV-FUNC-BDSCO       EQUAL SPACES) OR
              (GFCTFV-OPCAO            NOT NUMERIC)  OR
              (GFCTFV-OPCAO            NOT EQUAL 1 AND 2)
               MOVE 1                  TO GFCTFW-ERRO
                                          GFCTFW-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO GFCTFW-COD-SQL-ERRO
               PERFORM 111000-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTFW-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       220000-OBTER-DATA-SIST          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
               SELECT  CSIST_PRINC,
                       DPROCM_ANTER,
                       DPROCM_ATUAL,
                       DPROCM_PROX
               INTO   :GFCTB0A1.CSIST-PRINC,
                      :GFCTB0A1.DPROCM-ANTER,
                      :GFCTB0A1.DPROCM-ATUAL,
                      :GFCTB0A1.DPROCM-PROX
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       221000-TRATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0375'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          GFCTFW-COD-SQL-ERRO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO GFCTFW-ERRO.

           IF  GFCT0M-COMANDO-SQL      EQUAL 'SELECT'
               MOVE 0012               TO GFCTFW-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
           ELSE
               MOVE 0019               TO GFCTFW-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
           END-IF.

           PERFORM 111000-OBTER-DESC-MENSAGEM.

           MOVE SPACES                 TO GFCTFW-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '  '           INTO GFCTFW-DESC-MSG-ERRO.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       221000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       230000-CONSULTAR-GFCTB0A7       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFV-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A7.
           MOVE GFCTFV-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A7.
           MOVE GFCTFV-DATA-OCORR      TO DOCOR-EVNTO       OF GFCTB0A7.
           MOVE GFCTFV-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A7.
           EXEC SQL
               SELECT  CAGPTO_CTA,
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
               FROM    DB2PRD.PDIDO_ESTRN_GRP
               WHERE   CAGPTO_CTA         = :GFCTB0A7.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A7.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A7.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A7.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       240000-MOVER-CAMPOS             SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0A7 TO GFCTFW-CAGPTO.
           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO GFCTFW-CSERVC-TARIF.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO GFCTFW-HORA.
           MOVE DOCOR-EVNTO OF GFCTB0A7
                                       TO GFCTFW-DATA-OCORR.
           MOVE CINDCD-AGPTO-TOT OF GFCTB0A7
                                       TO GFCTFW-ESTORNO-TOTAL.
           MOVE DEFETV-CREDT-ESTRN OF GFCTB0A7
                                       TO GFCTFW-DATA-EFETIVACAO.

           STRING HIDTFD-ESTRN-AGPTO OF GFCTB0A7(09:02) '.'
                  HIDTFD-ESTRN-AGPTO OF GFCTB0A7(06:02) '.'
                  HIDTFD-ESTRN-AGPTO OF GFCTB0A7(01:04)
           DELIMITED BY SIZE           INTO GFCTFW-DATA-DEBITO.

           MOVE QESTRN-AGPTO OF GFCTB0A7
                                       TO GFCTFW-QESTORNOS.
           MOVE QCANCT-AGPTO OF GFCTB0A7
                                       TO GFCTFW-QCANCELAMENTOS.
           MOVE VESTRN-SOLTD OF GFCTB0A7
                                       TO GFCTFW-VAESTORNAR.
           MOVE VCANCT-SOLTD OF GFCTB0A7
                                       TO GFCTFW-VACANCELAR.
           MOVE RJUSTF-SOLTC-ESTRN-TEXT OF GFCTB0A7
                                       TO GFCTFW-COMENTARIO.

           PERFORM 241000-OBTER-DESC-AGRUPAMENTO.

           MOVE RAGPTO-CTA OF GFCTB0F3 TO GFCTFW-RAGPTO.

           PERFORM 242000-OBTER-DESC-TARIFA.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTFW-RSERVC-TARIF.

           EVALUATE GFCTFV-CAGPTO
               WHEN 15
                   PERFORM 243000-OBTER-POSTO
               WHEN 14
                   PERFORM 244000-OBTER-AGENCIA
               WHEN 16
                   PERFORM 245000-OBTER-EMPRESA
               WHEN 17
                   PERFORM 246000-OBTER-POSTAL
               WHEN 4
                   PERFORM 247000-OBTER-CLIENTE
               WHEN OTHER
                   PERFORM 248000-OBTER-SEGMENTO
           END-EVALUATE.

           MOVE CTPO-OPER-MOTVO OF GFCTB0A7
                                       TO GFCTFW-COPERACAO.

           PERFORM 249000-OBTER-DESC-OPERACAO.

           MOVE RTPO-OPER-MOTVO OF GFCTB0G0
                                       TO GFCTFW-ROPERACAO.
           MOVE CMOTVO-JUSTF OF GFCTB0A7
                                       TO GFCTFW-CMOTIVO.

           PERFORM 249100-OBTER-DESC-MOTIVO.

           MOVE RMOTVO-JUSTF OF GFCTB089
                                       TO GFCTFW-RMOTIVO.

      *----------------------------------------------------------------*
       240000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       241000-OBTER-DESC-AGRUPAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0A7 TO CAGPTO-CTA OF GFCTB0F3.

           EXEC SQL
               SELECT  RAGPTO_CTA
               INTO   :GFCTB0F3.RAGPTO-CTA
               FROM    DB2PRD.TPO_AGPTO_CTA
               WHERE   CAGPTO_CTA      = :GFCTB0F3.CAGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TPO_AGPTO_CTA'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0F3'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       241000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       242000-OBTER-DESC-TARIFA        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT  RSERVC_TARIF_REDZD
               INTO   :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF    = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       242000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       243000-OBTER-POSTO              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFV-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A8.
           MOVE GFCTFV-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A8.
           MOVE GFCTFV-DATA-OCORR      TO DOCOR-EVNTO       OF GFCTB0A8.
           MOVE GFCTFV-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A8.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO   :GFCTB0A8.CAGPTO-CTA,
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
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_PAB'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A8'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

           MOVE CDEPDC OF GFCTB0A8     TO WRK-COD-AGENCIA.

           PERFORM 244100-OBTER-DESC-AGENCIA.

           MOVE GFCT1X-DESC-DEPDC(1)   TO GFCTFW-RAGENCIA.

           MOVE CPOSTO-SERVC OF GFCTB0A8
                                       TO GFCTFW-CPAB.

           MOVE CDEPDC OF GFCTB0A8     TO GFCTFW-CAGENCIA.

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTFW-RPAB
           ELSE
               PERFORM 243100-OBTER-DESC-PAB

               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTFW-RPAB
           END-IF.

      *----------------------------------------------------------------*
       243000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       243100-OBTER-DESC-PAB           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTFV-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTFV-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTFV-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
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
               MOVE 9                  TO GFCTFW-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0375'         TO GFCT0M-TRANSACAO

               PERFORM 300000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTFW-FIM
               MOVE GFCTGC-ERRO        TO GFCTFW-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTFW-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTFW-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTFW-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTFW-DESC-MSG-ERRO
               END-IF

               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       243100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       244000-OBTER-AGENCIA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFV-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A3.
           MOVE GFCTFV-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A3.
           MOVE GFCTFV-DATA-OCORR      TO DOCOR-EVNTO       OF GFCTB0A3.
           MOVE GFCTFV-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A3.

           EXEC SQL
               SELECT  CAGPTO_CTA,
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
               FROM    DB2PRD.PDIDO_ESTRN_AG
               WHERE   CAGPTO_CTA         = :GFCTB0A3.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A3.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A3.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A3.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_AG'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A3'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

           MOVE CDEPDC OF GFCTB0A3     TO GFCTFW-CAGENCIA.

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODAS'            TO GFCTFW-RAGENCIA
           ELSE
               MOVE CDEPDC OF GFCTB0A3 TO WRK-COD-AGENCIA

               PERFORM 244100-OBTER-DESC-AGENCIA

               MOVE GFCT1X-DESC-DEPDC(1)
                                       TO GFCTFW-RAGENCIA
           END-IF.

      *----------------------------------------------------------------*
       244000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       244100-OBTER-DESC-AGENCIA       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.

           MOVE GFCTFV-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTFV-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTFV-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
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
               MOVE 9                  TO GFCTFW-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0375'         TO GFCT0M-TRANSACAO

               PERFORM 300000-FINALIZAR
           END-IF

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTFW-FIM
               MOVE GFCT1X-ERRO        TO GFCTFW-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTFW-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTFW-COD-MSG-ERRO

               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTFW-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'
                          GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTFW-DESC-MSG-ERRO
               END-IF

               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       244100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       245000-OBTER-EMPRESA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFV-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A6.
           MOVE GFCTFV-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A6.
           MOVE GFCTFV-DATA-OCORR      TO DOCOR-EVNTO       OF GFCTB0A6.
           MOVE GFCTFV-HORA            TO HIDTFD-ESTRN-AGPTO
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
               INTO   :GFCTB0A6.CAGPTO-CTA,
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
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_EMPR' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A6'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

           MOVE CAG-EMPR OF GFCTB0A6   TO GFCTFW-CAGENCIA.
           MOVE CNRO-CTA-DEB OF GFCTB0A6
                                       TO GFCTFW-CCONTA.

           PERFORM 245100-OBTER-DESC-EMPRESA.

           MOVE GFCTBS-NOME-CLIENTE(1) TO GFCTFW-RCONTA.

      *----------------------------------------------------------------*
       245000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       245100-OBTER-DESC-EMPRESA       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTBR-ENTRADA.

           MOVE 100                    TO GFCTBR-LL.
           MOVE ZEROS                  TO GFCTBR-ZZ.

           MOVE GFCTFV-TRANSACAO       TO GFCTBR-TRANSACAO.
           MOVE GFCTFV-FUNCAO          TO GFCTBR-FUNCAO.
           MOVE ZEROS                  TO GFCTBR-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTBR-QTDE-TOT-REG.
           MOVE GFCTFV-FUNC-BDSCO      TO GFCTBR-FUNC-BDSCO.
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
               MOVE 9                  TO GFCTFW-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0375'         TO GFCT0M-TRANSACAO

               PERFORM 300000-FINALIZAR
           END-IF

           IF  GFCTBS-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTFW-FIM
               MOVE GFCTBS-ERRO        TO GFCTFW-ERRO
               MOVE GFCTBS-COD-SQL-ERRO
                                       TO GFCTFW-COD-SQL-ERRO
               MOVE GFCTBS-COD-MSG-ERRO
                                       TO GFCTFW-COD-MSG-ERRO

               IF  GFCTBS-ERRO         EQUAL 1
                   MOVE GFCTBS-DESC-MSG-ERRO
                                       TO GFCTFW-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5060-'
                          GFCTBS-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTFW-DESC-MSG-ERRO
               END-IF

               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       245100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       246000-OBTER-POSTAL             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFV-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A9.
           MOVE GFCTFV-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A9.
           MOVE GFCTFV-DATA-OCORR      TO DOCOR-EVNTO       OF GFCTB0A9.
           MOVE GFCTFV-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A9.

           EXEC SQL
               SELECT  CAGPTO_CTA,
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
               FROM    DB2PRD.PDIDO_ESTRN_PSTAL
               WHERE   CAGPTO_CTA         = :GFCTB0A9.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A9.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A9.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A9.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_PSTAL'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A9'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

           MOVE CDEPDC OF GFCTB0A9     TO WRK-COD-AGENCIA.

           PERFORM 244100-OBTER-DESC-AGENCIA.

           MOVE GFCT1X-DESC-DEPDC(1)   TO GFCTFW-RAGENCIA.

           MOVE CPOSTO-SERVC OF GFCTB0A9
                                       TO GFCTFW-CPAB.

           MOVE CDEPDC OF GFCTB0A9     TO GFCTFW-CAGENCIA.

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTFW-RPAB
           ELSE
               PERFORM 246100-OBTER-DESC-POSTAL

               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTFW-RPAB
           END-IF.

      *----------------------------------------------------------------*
       246000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       246100-OBTER-DESC-POSTAL        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTFV-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTFV-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTFV-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
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
               MOVE 9                  TO GFCTFW-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0375'         TO GFCT0M-TRANSACAO

               PERFORM 300000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTFW-FIM
               MOVE GFCTGC-ERRO        TO GFCTFW-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTFW-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTFW-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTFW-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTFW-DESC-MSG-ERRO
               END-IF

               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       246100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       247000-OBTER-CLIENTE            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFV-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A4.
           MOVE GFCTFV-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A4.
           MOVE GFCTFV-DATA-OCORR      TO DOCOR-EVNTO       OF GFCTB0A4.
           MOVE GFCTFV-HORA            TO HIDTFD-ESTRN-AGPTO
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
               INTO   :GFCTB0A4.CAGPTO-CTA,
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
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A4'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

ST25X6*    MOVE CCGC-CPF OF GFCTB0A4   TO GFCTFW-CGC-CPF.
ST25X6*    MOVE CFLIAL-CGC OF GFCTB0A4 TO GFCTFW-FILIAL.
ST25X6*    MOVE CCTRL-CPF-CGC OF GFCTB0A4
ST25X6*                                TO GFCTFW-CONTROLE.
ST25X6     MOVE CCGC-CPF-ST OF GFCTB0A4 TO GFCTFW-CGC-CPF.
ST25X6     MOVE CFLIAL-CGC-ST OF GFCTB0A4 TO GFCTFW-FILIAL.
ST25X6     MOVE CCTRL-CPF-CGC-ST OF GFCTB0A4
ST25X6                                 TO GFCTFW-CONTROLE.

           PERFORM 247100-OBTER-DESC-CLIENTE.

           MOVE GFCTIV-NOME-CLIENTE    TO GFCTFW-RCONTA.

      *----------------------------------------------------------------*
       247000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       247100-OBTER-DESC-CLIENTE       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIU-ENTRADA.

           INITIALIZE                  GFCTIU-ENTRADA.

           MOVE 100                    TO GFCTIU-LL.
           MOVE ZEROS                  TO GFCTIU-ZZ.

           MOVE GFCTFV-TRANSACAO       TO GFCTIU-TRANSACAO.
           MOVE GFCTFV-FUNCAO          TO GFCTIU-FUNCAO.
           MOVE GFCTFV-FUNC-BDSCO      TO GFCTIU-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTIU-FIM.
ST25X6*    MOVE CCGC-CPF OF GFCTB0A4   TO GFCTIU-COD-CPF-CNPJ.
ST25X6*    MOVE CFLIAL-CGC OF GFCTB0A4 TO GFCTIU-COD-FILIAL.
ST25X6*    MOVE CCTRL-CPF-CGC OF GFCTB0A4
ST25X6*                                TO GFCTIU-CTRL-CPF-CNPJ.
ST25X6     MOVE CCGC-CPF-ST OF GFCTB0A4 TO GFCTIU-COD-CPF-CNPJ.
ST25X6     MOVE CFLIAL-CGC-ST OF GFCTB0A4 TO GFCTIU-COD-FILIAL.
ST25X6     MOVE CCTRL-CPF-CGC-ST OF GFCTB0A4
ST25X6                                 TO GFCTIU-CTRL-CPF-CNPJ.

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
               MOVE 9                  TO GFCTFW-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0375'         TO GFCT0M-TRANSACAO

               PERFORM 300000-FINALIZAR
           END-IF

           IF  GFCTIV-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTFW-FIM
               MOVE GFCTIV-ERRO        TO GFCTFW-ERRO
               MOVE GFCTIV-COD-SQL-ERRO
                                       TO GFCTFW-COD-SQL-ERRO
               MOVE GFCTIV-COD-MSG-ERRO
                                       TO GFCTFW-COD-MSG-ERRO

               IF  GFCTIV-ERRO         EQUAL 1
                   MOVE GFCTIV-DESC-MSG-ERRO
                                       TO GFCTFW-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5110-'
                          GFCTIV-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTFW-DESC-MSG-ERRO
               END-IF

               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       247100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       248000-OBTER-SEGMENTO           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFV-CAGPTO          TO CAGPTO-CTA        OF GFCTB0B0.
           MOVE GFCTFV-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B0.
           MOVE GFCTFV-DATA-OCORR      TO DOCOR-EVNTO       OF GFCTB0B0.
           MOVE GFCTFV-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0B0.

           EXEC SQL
               SELECT  CAGPTO_CTA,
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
               FROM    DB2PRD.PDIDO_ESTRN_SGMTO
               WHERE   CAGPTO_CTA         = :GFCTB0B0.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0B0.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0B0.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0B0.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0B0'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

           IF  WRK-CSGMTO-GSTAO-TARIF-NULL
                                       LESS ZEROS
               GO                      TO 248000-99-FIM
           END-IF.

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0B0
                                       TO GFCTFW-CSEGMENTO.

           PERFORM 248100-OBTER-DESC-SEGMENTO.

           MOVE ISGMTO-GSTAO-TARIF OF GFCTB0H5
                                       TO GFCTFW-RSEGMENTO.

      *----------------------------------------------------------------*
       248000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       248100-OBTER-DESC-SEGMENTO      SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0B0
                                       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0H5.

           EXEC SQL
               SELECT  ISGMTO_GSTAO_TARIF
               INTO   :GFCTB0H5.ISGMTO-GSTAO-TARIF
               FROM    DB2PRD.TTPO_SGMTO_GSTAO
               WHERE   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0H5'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       248100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       249000-OBTER-DESC-OPERACAO      SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO OF GFCTB0A7
                                       TO CTPO-OPER-MOTVO OF GFCTB0G0.

           EXEC SQL
               SELECT  RTPO_OPER_MOTVO
               INTO   :GFCTB0G0.RTPO-OPER-MOTVO
               FROM    DB2PRD.TPO_OPER_MOTVO
               WHERE   CTPO_OPER_MOTVO = :GFCTB0G0.CTPO-OPER-MOTVO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TPO_OPER_MOTVO'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0G0'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       249000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       249100-OBTER-DESC-MOTIVO        SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO OF GFCTB0A7
                                       TO CTPO-OPER-MOTVO OF GFCTB089.
           MOVE CMOTVO-JUSTF OF GFCTB0A7
                                       TO CMOTVO-JUSTF OF GFCTB089.

           EXEC SQL
               SELECT  RMOTVO_JUSTF
               INTO   :GFCTB089.RMOTVO-JUSTF
               FROM    DB2PRD.MOTVO_JUSTF_OPER
               WHERE   CTPO_OPER_MOTVO = :GFCTB089.CTPO-OPER-MOTVO
                 AND   CMOTVO_JUSTF    = :GFCTB089.CMOTVO-JUSTF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'MOTVO_JUSTF_OPER' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE 'GFCTB089'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       249100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       250000-EFETIVAR                 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFV-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A7.
           MOVE GFCTFV-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A7.
           MOVE GFCTFV-DATA-OCORR      TO DOCOR-EVNTO       OF GFCTB0A7.
           MOVE GFCTFV-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A7.
           MOVE 4                      TO CSIT-PROCM-ESTRN  OF GFCTB0A7.

           EXEC SQL
               UPDATE  DB2PRD.PDIDO_ESTRN_GRP
               SET     CSIT_PROCM_ESTRN   = :GFCTB0A7.CSIT-PROCM-ESTRN
               WHERE   CAGPTO_CTA         = :GFCTB0A7.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A7.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A7.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A7.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       250000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
