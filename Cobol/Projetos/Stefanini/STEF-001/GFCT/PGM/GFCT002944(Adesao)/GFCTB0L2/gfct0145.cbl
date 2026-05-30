      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0145.
       AUTHOR.     HELIO SANTONI NETO.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0144                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK - GP.50  *
      *    ANALISTA....:   WAGNER SILVA            - PROCWORK - GP.50  *
      *    DATA........:   05/03/2009                                  *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CONSULTA DETALHADA DO DEFERIMENTO, POR AGRUP. E TARIFA    *
      *      SELECIONADO.                                              *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *       DB2PRD.TCANCT_ADSAO_AGPTO              GFCTB0L0          *
      *       DB2PRD.TCANCT_ADSAO_EMPR               GFCTB0L1          *
      *       DB2PRD.TCANCT_ADSAO_FORNC              GFCTB0L2          *
      *       DB2PRD.TCANCT_ADSAO_POSTO              GFCTB0L3          *
      *       DB2PRD.CLIENTE_PAGADOR                 PFEBB000          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5500 - OBTER NOME DO CLIENTE                          *
      *      GFCT5006 - OBTER DESCRICAO DA AGENCIA                     *
      *      GFCT5060 - OBTER NOME DO CLIENTE                          *
      *      GFCT5090 - OBTER DESCRICAO DO CORRESPONDENTE BANCARIO     *
      *      GFCT5109 - OBTER DESCRICAO DO PAB                         *
      *      GFCT5110 - OBTER NOME DO CLIENTE POR CPF/CNPJ             *
      *      SARH8000 - MODULO PARA PERMITIR AOS SISTEMAS LEGADOS O    *
      *                 ACESSO AS INFORMACOES DE FUNCIONARIOS.         *
      *                                                                *
      *    NAVEGACAO:                                                  *
      *      CHAMADO POR: GFCT7148                                     *
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
           '*  INICIO DA WORKING GFCT0145  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*          AUXILIARES          *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.

       01  WRK-MSG-TAB.
           03  FILLER                  PIC X(03)           VALUE ' - '.
           03  WRK-NOME-TAB            PIC X(08)           VALUE SPACES.

       01  WRK-COD-AGENCIA-S           PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES     WRK-COD-AGENCIA-S.
           05  FILLER                  PIC  X(001).
           05  WRK-COD-AGENCIA         PIC  9(005).

       01  WRK-AUX-2-S                 PIC +9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-2-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-2               PIC  9(002).

       01  WRK-AUX-3-S                 PIC +9(003)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-3-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-3               PIC  9(003).

       01  WRK-AUX-5-S                 PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-5-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-5               PIC  9(005).

       01  WRK-AUX-11-S                PIC +9(011)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-11-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-11              PIC  9(011).

       01  WRK-AUX-13-S                PIC +9(013)         VALUE ZEROS.

       01  FILLER                      REDEFINES           WRK-AUX-13-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-13              PIC  9(013).
           05 FILLER                   REDEFINES WRK-AUX-13.
             10 FILLER                 PIC  X(006).
             10 WRK-AUX-13-7           PIC  9(007).

       01  WRK-AUX-9-S                 PIC +9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-9-S.
           05 FILLER                   PIC  X(001).
           05 WRK-AUX-9                PIC  9(009).
           05 FILLER                   REDEFINES WRK-AUX-9.
             10 FILLER                 PIC  X(002).
             10 WRK-AUX-9-7-A.
               15 WRK-AUX-9-7          PIC  9(007).

       01  WRK-SQLCODE-S               PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE-S.
           05 WRK-SQL-9                PIC  9(009).
           05 FILLER                   REDEFINES WRK-SQL-9.
              10 FILLER                PIC  X(006).
              10 FILLER.
                 15 WRK-SQL-3          PIC  9(003).

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
           'ERRO NA CHAMADA DO MODULO '.
           03  WRK-MSG05-MODULO        PIC X(08)           VALUE SPACES.

       01  WRK-HINCL.
           03  WRK-DATA.
               05  WRK-ANO             PIC 9(04)           VALUE ZEROS.
               05  FILLER              PIC X(01)           VALUE SPACES.
               05  WRK-MES             PIC 9(02)           VALUE ZEROS.
               05  FILLER              PIC X(01)           VALUE SPACES.
               05  WRK-DIA             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE SPACES.
           03  WRK-HORA.
               05  WRK-HH              PIC 9(02)           VALUE ZEROS.
               05  WRK-PT              PIC X(01)           VALUE SPACES.
               05  WRK-MM              PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(10)           VALUE SPACES.

       01  WRK-DATA-AUX.
           03  WRK-DIA-AUX             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-MES-AUX             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-ANO-AUX             PIC 9(04)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE CAMPOS COM NULIDADE'.
      *----------------------------------------------------------------*

       01  WRK-PFEBB000-NULL.
           03 WRK-ICLI-NULL            PIC S9(04) COMP VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM SARH8000'.
      *----------------------------------------------------------------*

       01 WRK-ENTRADA.
          05 WRK-PESQUISA-ENT          PIC 9(009)          VALUE ZEROS.
          05 WRK-TAMANHO-ENT           PIC 9(004)          VALUE ZEROS.
          05 WRK-SOLICITA-ENT.
            10 FILLER                  OCCURS 255 TIMES.
              15 WRK-CAMPO-ENT         PIC 9(003)          VALUE ZEROS.

       01 WRK-SAIDA.
          05 WRK-COD-RETORNO-SAI       PIC 9(002)          VALUE ZEROS.
          05 WRK-TAMANHO-SAI           PIC 9(004)          VALUE ZEROS.
          05 WRK-REGISTRO-SAI.
            10 FILLER                  OCCURS 0 TO 1000 TIMES
                                       DEPENDING ON WRK-TAMANHO-SAI.
              15 WRK-POSICAO-SAI       PIC X(001)          VALUE SPACES.

       01  WRK-AREA-RET.
           05 WRK-NOME-RET             PIC  X(040)         VALUE SPACES.
           05 FILLER                   PIC  X(040)         VALUE SPACES.

       01  WRK-ERRO-AREA               PIC  X(107)         VALUE SPACES.
       01  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

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
       01  FILLER                      PIC X(60)           VALUE
           '*    AREA DO MODULO GFCT5090 E GFCT5109  *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGB'.
       COPY 'I#GFCTGC'.

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
           '*      AREA DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L3
           END-EXEC.

           EXEC SQL
                INCLUDE PFEBB000
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT0145   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAFL.
       COPY GFCTWAFM.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION                       USING WAFL-ENTRADA
                                                      WAFM-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *  ROTINA PRINCIPAL DO PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  INICIALIZACAO DO PROGRAMA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-MONTAR-AREA-SAIDA

           PERFORM 1200-VERIFICAR-ONLINE

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  MONTA AREA DE SAIDA                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAFM-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  WAFM-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 1010                   TO WAFM-LL.

           MOVE WAFL-TRANSACAO         TO WAFM-TRANSACAO.
           MOVE WAFL-FUNCAO            TO WAFM-FUNCAO.
           MOVE WAFL-FUNC-BDSCO        TO WAFM-FUNC-BDSCO.
           MOVE WAFL-TERMINAL          TO WAFM-TERMINAL.
           MOVE 'S'                    TO WAFM-FIM.
           MOVE ZEROS                  TO WAFM-ERRO
                                          WAFM-COD-SQL-ERRO
                                          WAFM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           PERFORM 1110-OBTER-DESC-MENSAGEM.

           MOVE WRK-5000-DESC-MSG      TO WAFM-DESC-MSG-ERRO.

           MOVE WAFL-OPCAO             TO WAFM-OPCAO
           MOVE WAFL-HINCL             TO WAFM-HINCL
           MOVE WAFL-CAGPTO            TO WAFM-CAGPTO
           MOVE WAFL-RAGPTO            TO WAFM-RAGPTO
           MOVE WAFL-CPACOTE           TO WAFM-CPACOTE
           MOVE WAFL-RPACOTE           TO WAFM-RPACOTE
           MOVE WAFL-CAGENCIA          TO WAFM-CAGENCIA

           MOVE WAFL-RAGENCIA          TO WAFM-RAGENCIA
           MOVE WAFL-CPOSTO            TO WAFM-CPOSTO
           MOVE WAFL-RPOSTO            TO WAFM-RPOSTO
           MOVE WAFL-CCONTA            TO WAFM-CCONTA
           MOVE WAFL-RCONTA            TO WAFM-RCONTA
ST2506*    MOVE WAFL-CNPJ-NUM          TO WAFM-CNPJ-NUM
ST2506     MOVE WAFL-CNPJ-CGC          TO WAFM-CNPJ-CGC
ST2506     MOVE WAFL-CNPJ-FIL          TO WAFM-CNPJ-FIL
           MOVE WAFL-CNPJ-CTR          TO WAFM-CNPJ-CTR
           MOVE WAFL-RCNPJ             TO WAFM-RCNPJ
           MOVE WAFL-DATA-SOLIC        TO WAFM-DATA-SOLIC
           MOVE WAFL-COMENT            TO WAFM-COMENT
           MOVE WAFL-CFUNC-SOLIC       TO WAFM-CFUNC-SOLIC
           MOVE WAFL-RFUNC-SOLIC       TO WAFM-RFUNC-SOLIC
           MOVE WAFL-DATA-INCL         TO WAFM-DATA-INCL
           MOVE WAFL-HORA-INCL         TO WAFM-HORA-INCL
           MOVE WAFL-QTD-PREVIA        TO WAFM-QTD-PREVIA
           MOVE WAFL-PARECER           TO WAFM-PARECER
           MOVE WAFL-JUSTIF            TO WAFM-JUSTIF.

      *----------------------------------------------------------------*
       1100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER DESCRICAO DA MENSAGEM                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-OBTER-DESC-MENSAGEM       SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING WRK-5000-AREA-ENVIA
                                          WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO   NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                    EQUAL 01
                   MOVE 1              TO WAFM-ERRO
                   MOVE ZEROS          TO WAFM-COD-SQL-ERRO
                                          WAFM-COD-MSG-ERRO

                   MOVE WRK-5000-DESC-MSG
                                       TO WAFM-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                    EQUAL 99
                   MOVE 1              TO WAFM-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO WAFM-COD-SQL-ERRO
                   MOVE ZEROS          TO WAFM-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO WAFM-DESC-MSG-ERRO
               END-IF
               INITIALIZE              WAFM-DADOS-REST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************

      *  VERIFICA DISPONIBILIDADE ONLINE                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-VERIFICAR-ONLINE          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5500-AREA-SAIDA.

           MOVE 'GFCT5500'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING WRK-5500-AREA-SAIDA.

           IF  WRK-5500-COD-RETORNO   NOT EQUAL ZEROS
               IF  WRK-5500-COD-RETORNO
                                    EQUAL 04
                   MOVE 1              TO WAFM-ERRO
                   MOVE ZEROS          TO WAFM-COD-SQL-ERRO
                   MOVE 0030           TO WAFM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 1110-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO WAFM-DESC-MSG-ERRO
                   INITIALIZE             WAFM-DADOS-REST
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   MOVE SPACES         TO GFCT0M-AREA-ERROS
                   INITIALIZE          GFCT0M-AREA-ERROS
                   MOVE 'GFCT0145'     TO GFCT0M-PROGRAMA
                   MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'PARM_CTRL_GSTAO'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          WAFM-COD-SQL-ERRO
                   MOVE '0010'         TO GFCT0M-LOCAL
                   MOVE 9              TO WAFM-ERRO
                   MOVE 0012           TO WAFM-COD-MSG-ERRO
                   MOVE WRK-5500-DESC-MSG
                                       TO WAFM-DESC-MSG-ERRO

                   INITIALIZE          WAFM-DADOS-REST
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER CONSISTENCIA DE CAMPOS                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (WAFL-TRANSACAO        EQUAL SPACES)       OR
              (WAFL-FUNCAO           EQUAL SPACES)       OR
              (WAFL-FUNC-BDSCO       EQUAL SPACES)       OR
              (WAFL-OPCAO            NOT NUMERIC)        OR
              (WAFL-OPCAO            NOT EQUAL '1' AND '2')  OR
              (WAFL-CAGPTO           NOT NUMERIC)        OR
              (WAFL-CAGPTO           NOT EQUAL  15 AND 16 AND
                                                17 AND 20 AND 22 )
               MOVE 1                  TO WAFM-ERRO
                                          WAFM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO WAFM-COD-SQL-ERRO
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WAFM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAMENTO PRINCIPAL                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           IF  WAFL-OPCAO            EQUAL '1'
               PERFORM 2100-CONSULTAR
               PERFORM 2200-CARREGAR-SAIDA
               PERFORM 2300-OBTER-DADOS-CAGPTO
           ELSE
               PERFORM 2400-EFETIVAR-DEFERIMENTO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  CONSULTA DE ADESAO AGRUPAMENTO                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSULTAR                 SECTION.
      *----------------------------------------------------------------*

           MOVE WAFL-CAGPTO            TO CAGPTO-CTA   OF GFCTB0L0.
           MOVE WAFL-CPACOTE           TO CSERVC-TARIF OF GFCTB0L0.
           MOVE WAFL-HINCL             TO HINCL-REG    OF GFCTB0L0.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       HINCL_REG,
                       CFUNC_SOLTC_CANCT,
                       CFUNC_MANUT,
                       CSIT_SOLTC_CANCT,
                       QPRVIO_CANCT_ADSAO,
                       QADSAO_CANCD,
                       HEXCUC_CALC_PRVIO,
                       HEXCUC_PROCM,
                       HMANUT_REG,
                       RJUSTF_SOLTC_CANCT,
                       RJUSTF_PAREC_CANCT
                INTO   :GFCTB0L0.CAGPTO-CTA,
                       :GFCTB0L0.CSERVC-TARIF,
                       :GFCTB0L0.HINCL-REG,
                       :GFCTB0L0.CFUNC-SOLTC-CANCT,
                       :GFCTB0L0.CFUNC-MANUT,
                       :GFCTB0L0.CSIT-SOLTC-CANCT,
                       :GFCTB0L0.QPRVIO-CANCT-ADSAO,
                       :GFCTB0L0.QADSAO-CANCD,
                       :GFCTB0L0.HEXCUC-CALC-PRVIO,
                       :GFCTB0L0.HEXCUC-PROCM,
                       :GFCTB0L0.HMANUT-REG,
                       :GFCTB0L0.RJUSTF-SOLTC-CANCT,
                       :GFCTB0L0.RJUSTF-PAREC-CANCT
                FROM   DB2PRD.TCANCT_ADSAO_AGPTO
                WHERE  CAGPTO_CTA      = :GFCTB0L0.CAGPTO-CTA   AND
                       CSERVC_TARIF    = :GFCTB0L0.CSERVC-TARIF AND
                       HINCL_REG       = :GFCTB0L0.HINCL-REG
           END-EXEC.

           IF (SQLCODE                NOT EQUAL ZEROS) OR
              (SQLWARN0             EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE                 GFCT0M-AREA-ERROS
               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE-S
               MOVE WRK-SQL-3          TO WAFM-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO WAFM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0L0'         TO WRK-NOME-TAB
               PERFORM 2110-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  MOVER ERRO SQL                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0145'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 9                      TO WAFM-ERRO.

           PERFORM 1110-OBTER-DESC-MENSAGEM.

           STRING WRK-5000-DESC-MSG
                  WRK-MSG-TAB
           DELIMITED BY '  '         INTO WAFM-DESC-MSG-ERRO.

           INITIALIZE              WAFM-DADOS-REST

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       2110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  CARREGA AREA DE SAIDA                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CARREGAR-SAIDA            SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO WRK-AUX-3-S
           MOVE WRK-AUX-3              TO WAFM-CAGPTO
           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO WAFM-CPACOTE
           MOVE HINCL-REG OF GFCTB0L0  TO WAFM-HINCL
                                          WRK-HINCL
           MOVE WRK-ANO                TO WRK-ANO-AUX
           MOVE WRK-MES                TO WRK-MES-AUX
           MOVE WRK-DIA                TO WRK-DIA-AUX
           MOVE WRK-DATA-AUX           TO WAFM-DATA-SOLIC
                                          WAFM-DATA-INCL
           MOVE '.'                    TO WRK-PT
           MOVE WRK-HORA               TO WAFM-HORA-INCL

           MOVE RJUSTF-SOLTC-CANCT-TEXT OF GFCTB0L0 (1:225)
                                       TO WAFM-COMENT
           MOVE QPRVIO-CANCT-ADSAO     OF GFCTB0L0
                                       TO WRK-AUX-11-S
           MOVE WRK-AUX-11             TO WAFM-QTD-PREVIA
           MOVE RJUSTF-PAREC-CANCT-TEXT OF GFCTB0L0 (1:255)
                                       TO WAFM-JUSTIF
           MOVE CFUNC-SOLTC-CANCT      OF GFCTB0L0
                                       TO WRK-AUX-9-S
           MOVE WRK-AUX-9              TO WRK-PESQUISA-ENT
           MOVE WRK-AUX-9-7-A          TO WAFM-CFUNC-SOLIC

           PERFORM 2210-OBTER-NOME-FUNC

           MOVE WRK-NOME-RET (1:30)    TO WAFM-RFUNC-SOLIC.

      *----------------------------------------------------------------*
       2200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER NOME DO FUNCIONARIO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-OBTER-NOME-FUNC           SECTION.
      *----------------------------------------------------------------*

           MOVE 0086                   TO WRK-TAMANHO-ENT.
           MOVE 002                    TO WRK-CAMPO-ENT(1).
           MOVE 'SARH8000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-ENTRADA
                                             WRK-SAIDA
                                             WRK-ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-COD-RETORNO-SAI  EQUAL 02
               MOVE 'FUNCIONARIO NAO ENCONTRADO '
                                       TO WRK-NOME-RET
               GO TO 2210-99-FIM
           END-IF.

           IF  WRK-COD-RETORNO-SAI     EQUAL ZEROS
               MOVE WRK-REGISTRO-SAI(1:80)
                                       TO WRK-AREA-RET
               GO TO 2210-99-FIM
           END-IF.

           IF  WRK-COD-RETORNO-SAI     EQUAL 10 OR 12
               MOVE ALL '*'            TO WRK-NOME-RET
           ELSE
               MOVE SPACES             TO WRK-NOME-RET
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DADOS DO AGRUPAMENTO SELECIONADO                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-OBTER-DADOS-CAGPTO        SECTION.
      *----------------------------------------------------------------*

           EVALUATE WAFL-CAGPTO
               WHEN 15
                   PERFORM 2310-OBTER-POSTO
               WHEN 17
                   PERFORM 2310-OBTER-POSTO
               WHEN 20
                   PERFORM 2310-OBTER-POSTO
               WHEN 16
                   PERFORM 2320-OBTER-EMPRESA
               WHEN OTHER
                   PERFORM 2330-OBTER-FORNECEDOR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER DADOS DO POSTO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-OBTER-POSTO               SECTION.
      *----------------------------------------------------------------*

           MOVE WAFL-CAGPTO            TO CAGPTO-CTA   OF GFCTB0L3.
           MOVE WAFL-CPACOTE           TO CSERVC-TARIF OF GFCTB0L3.
           MOVE WAFL-HINCL             TO HINCL-REG    OF GFCTB0L3.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       HINCL_REG,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO    :GFCTB0L3.CAGPTO-CTA,
                       :GFCTB0L3.CSERVC-TARIF,
                       :GFCTB0L3.HINCL-REG,
                       :GFCTB0L3.CDEPDC,
                       :GFCTB0L3.CPOSTO-SERVC
               FROM    DB2PRD.TCANCT_ADSAO_POSTO
               WHERE   CAGPTO_CTA   = :GFCTB0L3.CAGPTO-CTA
                 AND   CSERVC_TARIF = :GFCTB0L3.CSERVC-TARIF
                 AND   HINCL_REG    = :GFCTB0L3.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TCANCT_ADSAO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE-S
               MOVE WRK-SQL-3          TO WAFM-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO WAFM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0L3'         TO WRK-NOME-TAB

               PERFORM 2110-MOVER-ERRO-SQL
           END-IF.

           MOVE CDEPDC OF GFCTB0L3     TO WRK-COD-AGENCIA-S
           MOVE WRK-COD-AGENCIA        TO WAFM-CAGENCIA

           PERFORM 2311-OBTER-DESC-AGENCIA

           MOVE GFCT1X-DESC-DEPDC(1) (1:30)
                                       TO WAFM-RAGENCIA

           MOVE CPOSTO-SERVC OF GFCTB0L3
                                       TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO WAFM-CPOSTO

           IF  WAFL-CAGPTO          EQUAL 15
               PERFORM 2312-OBTER-DESC-PAB
           ELSE
               PERFORM 2313-OBTER-DESC-CBANC
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)   TO WAFM-RPOSTO.

      *----------------------------------------------------------------*
       2310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DESCRICAO AGENCIA                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2311-OBTER-DESC-AGENCIA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.

           MOVE WAFL-TRANSACAO         TO GFCT1V-TRANSACAO.
           MOVE WAFL-FUNCAO            TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.

           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE WAFL-FUNC-BDSCO        TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE WRK-COD-AGENCIA        TO GFCT1V-COD-DEPDC(1).

           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT1X-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE 9                  TO WAFM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0378'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAFM-FIM
               MOVE GFCT1X-ERRO        TO WAFM-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO WAFM-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO WAFM-COD-MSG-ERRO

               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO WAFM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'
                          GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE INTO WAFM-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2311-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DESCRICAO PAB                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2312-OBTER-DESC-PAB            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE WAFL-TRANSACAO         TO GFCTGB-TRANSACAO.
           MOVE WAFL-FUNCAO            TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE WAFL-FUNC-BDSCO        TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC OF GFCTB0L3     TO WRK-COD-AGENCIA-S
           MOVE WRK-COD-AGENCIA        TO WAFM-CAGENCIA
                                          GFCTGB-COD-DEPDC

           MOVE CPOSTO-SERVC OF GFCTB0L3
                                       TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO GFCTGB-COD-POSTO

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.


           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0378'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS

               MOVE 'S'                TO WAFM-FIM
               MOVE GFCTGC-ERRO        TO WAFM-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO WAFM-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO WAFM-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO WAFM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFM-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2312-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DESCRICAO CORRESPONDENTE BANCARIO                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2313-OBTER-DESC-CBANC          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE WAFL-TRANSACAO         TO GFCTGB-TRANSACAO.
           MOVE WAFL-FUNCAO            TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE WAFL-FUNC-BDSCO        TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.


           MOVE CDEPDC OF GFCTB0L3     TO WRK-COD-AGENCIA-S
           MOVE WRK-COD-AGENCIA        TO WAFM-CAGENCIA
                                          GFCTGB-COD-DEPDC

           MOVE CPOSTO-SERVC OF GFCTB0L3
                                       TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO GFCTGB-COD-POSTO

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS

               MOVE 9                  TO WAFM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0378'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAFM-FIM
               MOVE GFCTGC-ERRO        TO WAFM-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO WAFM-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO WAFM-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO WAFM-DESC-MSG-ERRO

               ELSE
                   STRING 'GFCT5109-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFM-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2313-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DADOS DA EMPRESA                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-OBTER-EMPRESA             SECTION.
      *----------------------------------------------------------------*

           MOVE WAFL-CAGPTO            TO CAGPTO-CTA   OF GFCTB0L1.
           MOVE WAFL-CPACOTE           TO CSERVC-TARIF OF GFCTB0L1.
           MOVE WAFL-HINCL             TO HINCL-REG    OF GFCTB0L1.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       HINCL_REG,
                       CAG_BCRIA,
                       CCTA_BCRIA_CLI
               INTO    :GFCTB0L1.CAGPTO-CTA,
                       :GFCTB0L1.CSERVC-TARIF,
                       :GFCTB0L1.HINCL-REG,
                       :GFCTB0L1.CAG-BCRIA,
                       :GFCTB0L1.CCTA-BCRIA-CLI
               FROM    DB2PRD.TCANCT_ADSAO_EMPR
               WHERE   CAGPTO_CTA   = :GFCTB0L1.CAGPTO-CTA
                 AND   CSERVC_TARIF = :GFCTB0L1.CSERVC-TARIF
                 AND   HINCL_REG    = :GFCTB0L1.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TCANCT_ADSAO_EMPR'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE-S
               MOVE WRK-SQL-3          TO WAFM-COD-SQL-ERRO
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO WAFM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0L1'         TO WRK-NOME-TAB
               PERFORM 2110-MOVER-ERRO-SQL
           END-IF.

           MOVE CAG-BCRIA OF GFCTB0L1  TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO WAFM-CAGENCIA
           MOVE CCTA-BCRIA-CLI         OF GFCTB0L1
                                       TO WRK-AUX-13-S

           MOVE WRK-AUX-13             TO WAFM-CCONTA.

           PERFORM 2321-OBTER-DESC-EMPRESA.

           MOVE GFCTBS-NOME-CLIENTE(1) (1:40)
                                       TO WAFM-RCONTA.

      *----------------------------------------------------------------*
       2320-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DESCRICAO DA EMPRESA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2321-OBTER-DESC-EMPRESA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTBR-ENTRADA.

           MOVE 100                    TO GFCTBR-LL.

           MOVE ZEROS                  TO GFCTBR-ZZ.

           MOVE WAFL-TRANSACAO         TO GFCTBR-TRANSACAO.
           MOVE WAFL-FUNCAO            TO GFCTBR-FUNCAO.
           MOVE ZEROS                  TO GFCTBR-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTBR-QTDE-TOT-REG.
           MOVE WAFL-FUNC-BDSCO        TO GFCTBR-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTBR-FIM.

           MOVE CAG-BCRIA OF GFCTB0L1  TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO GFCTBR-COD-DEPDC
           MOVE CCTA-BCRIA-CLI         OF GFCTB0L1
                                       TO WRK-AUX-13-S
           MOVE WRK-AUX-13-7           TO GFCTBR-CCTA-CLI.

           MOVE SPACES                 TO GFCTBS-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTBS-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5060'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCTBR-ENTRADA
                                          GFCTBS-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0145'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF

           IF  GFCTBS-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAFM-FIM
               MOVE GFCTBS-ERRO        TO WAFM-ERRO
               MOVE GFCTBS-COD-SQL-ERRO
                                       TO WAFM-COD-SQL-ERRO
               MOVE GFCTBS-COD-MSG-ERRO
                                       TO WAFM-COD-MSG-ERRO

               IF  GFCTBS-ERRO         EQUAL 1
                   MOVE GFCTBS-DESC-MSG-ERRO
                                       TO WAFM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5060-'
                          GFCTBS-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFM-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2321-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DADOS DO FORNECEDOR                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-OBTER-FORNECEDOR          SECTION.
      *----------------------------------------------------------------*

           MOVE WAFL-CAGPTO            TO CAGPTO-CTA   OF GFCTB0L2.
           MOVE WAFL-CPACOTE           TO CSERVC-TARIF OF GFCTB0L2.
           MOVE WAFL-HINCL             TO HINCL-REG    OF GFCTB0L2.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       HINCL_REG,
ST2506*                CCNPJ_CPF,
ST2506*                CFLIAL_CNPJ,
ST2506*                CCTRL_CNPJ
ST2506                 CCNPJ_CPF_ST,
ST2506                 CFLIAL_CNPJ_ST,
ST2506                 CCTRL_CNPJ_ST
               INTO    :GFCTB0L2.CAGPTO-CTA,
                       :GFCTB0L2.CSERVC-TARIF,
                       :GFCTB0L2.HINCL-REG,
ST2506*                :GFCTB0L2.CCNPJ-CPF,
ST2506*                :GFCTB0L2.CFLIAL-CNPJ,
ST2506*                :GFCTB0L2.CCTRL-CNPJ
ST2506                 :GFCTB0L2.CCNPJ-CPF-ST,
ST2506                 :GFCTB0L2.CFLIAL-CNPJ-ST,
ST2506                 :GFCTB0L2.CCTRL-CNPJ-ST
               FROM    DB2PRD.TCANCT_ADSAO_FORNC
               WHERE   CAGPTO_CTA   = :GFCTB0L2.CAGPTO-CTA
                 AND   CSERVC_TARIF = :GFCTB0L2.CSERVC-TARIF
                 AND   HINCL_REG    = :GFCTB0L2.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TCANCT_ADSAO_FORNC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE-S
               MOVE WRK-SQL-3          TO WAFM-COD-SQL-ERRO
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO WAFM-COD-MSG-ERRO

                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0L2'         TO WRK-NOME-TAB
               PERFORM 2110-MOVER-ERRO-SQL
           END-IF.

ST2506*    MOVE CCNPJ-CPF  OF GFCTB0L2 TO WRK-AUX-9-S
ST2506*    MOVE WRK-AUX-9              TO WAFM-CNPJ-NUM
ST2506     MOVE CCNPJ-CPF-ST           OF GFCTB0L2 
ST2506                                 TO WAFM-CNPJ-CGC
ST2506*    MOVE CFLIAL-CNPJ            OF GFCTB0L2
ST2506*                                TO WRK-AUX-5-S
ST2506*    MOVE WRK-AUX-5              TO WAFM-CNPJ-FIL
ST2506     MOVE CFLIAL-CNPJ-ST         OF GFCTB0L2
ST2506                                 TO WAFM-CNPJ-FIL
ST2506*    MOVE CCTRL-CNPJ OF GFCTB0L2 TO WRK-AUX-2-S
ST2506     MOVE CCTRL-CNPJ-ST          OF GFCTB0L2
ST2506                                 TO WRK-AUX-2-S
           MOVE WRK-AUX-2              TO WAFM-CNPJ-CTR

           PERFORM 2331-OBTER-CLIE-PAGDOR

           MOVE ICLI OF PFEBB000       TO WAFM-RCNPJ.

      *----------------------------------------------------------------*
       2330-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DESCRICAO DO CLIENTE PAGDOR                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2331-OBTER-CLIE-PAGDOR         SECTION.
      *----------------------------------------------------------------*
           
      * TODO - ALTERAR TABELA DO PFEBB000 PARA A TABELA DE CLIENTE_PAGADOR     
ST2506*    MOVE CCNPJ-CPF  OF GFCTB0L2 TO CCGC-CPF     OF PFEBB000
ST2506*    MOVE CFLIAL-CNPJ            OF GFCTB0L2
ST2506*                                TO CFLIAL-CGC   OF PFEBB000
ST2506*    MOVE CCTRL-CNPJ OF GFCTB0L2 TO CCTRL-CGC    OF PFEBB000
ST2506     MOVE CCNPJ-CPF-ST           OF GFCTB0L2 
ST2506                                 TO CCGC-CPF     OF PFEBB000
ST2506     MOVE CFLIAL-CNPJ-ST         OF GFCTB0L2
ST2506                                 TO CFLIAL-CGC   OF PFEBB000
ST2506     MOVE CCTRL-CNPJ-ST          OF GFCTB0L2
ST2506                                 TO CCTRL-CGC    OF PFEBB000

           EXEC SQL
              SELECT   ICLI
                INTO   :PFEBB000.ICLI
                       :WRK-ICLI-NULL
                FROM   DB2PRD.CLIENTE_PAGADOR
               WHERE   CCGC_CPF        = :PFEBB000.CCGC-CPF
                 AND   CFLIAL_CGC      = :PFEBB000.CFLIAL-CGC
                 AND   CCTRL_CGC       = :PFEBB000.CCTRL-CGC
           END-EXEC.


           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'CLIENTE_PAGADOR ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE-S
               MOVE WRK-SQL-3          TO WAFM-COD-SQL-ERRO
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0012               TO WAFM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'PFEBB000'         TO WRK-NOME-TAB
               PERFORM 2110-MOVER-ERRO-SQL
           END-IF.

           IF  WRK-ICLI-NULL         LESS ZEROS
               MOVE SPACES             TO ICLI  OF PFEBB000
           END-IF.

      *----------------------------------------------------------------*
       2331-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ATUALIZA TABELA DE DEFERIMENTO                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-EFETIVAR-DEFERIMENTO      SECTION.
      *----------------------------------------------------------------*

           MOVE WAFL-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L0
           MOVE WAFL-CPACOTE           TO CSERVC-TARIF       OF GFCTB0L0
           MOVE WAFL-HINCL             TO HINCL-REG          OF GFCTB0L0

           MOVE WAFL-JUSTIF            TO RJUSTF-PAREC-CANCT-TEXT
                                                             OF GFCTB0L0
           MOVE LENGTH OF RJUSTF-PAREC-CANCT-TEXT            OF GFCTB0L0
                                       TO RJUSTF-PAREC-CANCT-LEN
                                                             OF GFCTB0L0

           MOVE WAFL-FUNC-BDSCO        TO WRK-AUX-9-7-A
           MOVE WRK-AUX-9-7            TO CFUNC-MANUT        OF GFCTB0L0

           IF  WAFL-PARECER         EQUAL 1
               MOVE 4                  TO CSIT-SOLTC-CANCT   OF GFCTB0L0
           ELSE
               MOVE 3                  TO CSIT-SOLTC-CANCT   OF GFCTB0L0
           END-IF.

           EXEC SQL
               UPDATE DB2PRD.TCANCT_ADSAO_AGPTO
                 SET  CSIT_SOLTC_CANCT   = :GFCTB0L0.CSIT-SOLTC-CANCT,
                      CFUNC_MANUT        = :GFCTB0L0.CFUNC-MANUT,
                      HMANUT_REG         =  CURRENT TIMESTAMP,
                      RJUSTF_PAREC_CANCT = :GFCTB0L0.RJUSTF-PAREC-CANCT
               WHERE  CAGPTO_CTA         = :GFCTB0L0.CAGPTO-CTA
                 AND  CSERVC_TARIF       = :GFCTB0L0.CSERVC-TARIF
                 AND  HINCL_REG          = :GFCTB0L0.HINCL-REG
                 AND  CSIT_SOLTC_CANCT   = 2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE-S
               MOVE WRK-SQL-3          TO WAFM-COD-SQL-ERRO
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 0019               TO WAFM-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0L0'         TO WRK-NOME-TAB
               PERFORM 2110-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                   EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      *  FINALIZACAO DO PROGRAMA                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
