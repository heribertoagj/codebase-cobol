      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0292.
       AUTHOR.     ROBSON FAUSTO CESA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0292                                    *
      *    PROGRAMADOR.:   ROBSON FAUSTO CESA      - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE ALEX SOMMER      - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK /GP.50   *
      *    DATA........:   27/04/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA SUSPENSAO DE TARIFA. LISTAR ATE 5  *
      *      OCORRENCIAS NA SAIDA E MONTAR RESTART, CASO HAJA, COM A   *
      *      CHAVE DA PROXIMA OCORRENCIA VALIDA DO CURSOR.             *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.SUSP_COMP_PCOTE        GFCTB0E7       *
      *                   DB2PRD.SUSP_GRP_DEPDC         GFCTB0E4       *
      *                   DB2PRD.SUSP_GRP_MUN           GFCTB0E5       *
      *                   DB2PRD.TSUSP_GRP_PAB          GFCTB0I1       *
      *                   DB2PRD.SUSP_GRP_UF            GFCTB0E6       *
      *                   DB2PRD.SUSP_CLI               GFCTB0E3       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTPA - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTPB - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTGC - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCT1X - AREA DE COMUNICACAO - SAIDA                      *
      *    I#RURC88 - AREA PARA MODULO RURC9020                        *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    GFCT5109 - OBTER DESCRICAO DE PAB              .            *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5006 - DESCRICAO DEPENDENCIA                            *
      *    DCIT8000 - OBTEM UF                                         *
      *    RURC9020 - OBTEM MUNICIPIO                                  *
      *    SENH0315 - OBTER JUNCAO                                     *
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
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(001)         VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(008)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.
       77  W-DE20                      PIC  X(023)         VALUE SPACES.

       01  WRK-POOL7100                PIC  X(107)         VALUE SPACES.
       01  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

       01  WRK-DPROCM-ATUAL            PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DPROCM-ATUAL.
           05  WRK-DPROCM-ATUAL-ANO    PIC  9(004).
           05  WRK-DPROCM-ATUAL-MES    PIC  9(002).
           05  WRK-DPROCM-ATUAL-DIA    PIC  9(002).

       01  WRK-DFIM-SUSP-COBR          PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DFIM-SUSP-COBR.
           05  WRK-DFIM-SUSP-COBR-ANO  PIC  9(004).
           05  WRK-DFIM-SUSP-COBR-MES  PIC  9(002).
           05  WRK-DFIM-SUSP-COBR-DIA  PIC  9(002).

       01  WRK-CSERVC-TARIF-FILTRO     PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-CSERVC-TARIF-INI        PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-CSERVC-TARIF-FIM        PIC S9(005) COMP-3  VALUE ZEROS.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM DCIT8000 *'.
      *---------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002 PIC X(070)         VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO  PIC 9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM SENH0315 *'.
      *---------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           05  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
           05  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           05  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
           05  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
           05  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
           05  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5507 *'.
      *---------------------------------------------------------------*

       01  WRK-5507-AREA.
           05  WRK-5507-AREA-ENVIA.
               10  WRK-5507-AMBIENTE         PIC  X(001)  VALUE SPACES.
               10  WRK-5507-CJUNC-DEPDC-X.
                   15  WRK-5507-CJUNC-DEPDC  PIC  9(005)  VALUE ZEROS.
           05  WRK-5507-AREA-RECEBE.
               10  WRK-5507-COD-RETORNO      PIC  9(002)  VALUE ZEROS.
               10  WRK-5507-COD-SQL-ERRO     PIC  9(003)  VALUE ZEROS.
               10  WRK-5507-MSG-RETORNO      PIC  X(045)  VALUE SPACES.
               10  WRK-5507-IDTFD-MASTER     PIC  X(001)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA INCLUDES  *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#RURC88'.

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E4
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E5
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0I1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E3
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *---------------------------------------------------------------*

           EXEC SQL
             DECLARE   CSR01-GFCTB0I1   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_SUSP_COBR,
                   CSEQ_AGPTO_CTA,
                   CDEPDC,
                   CPOSTO_SERVC
             FROM  DB2PRD.TSUSP_GRP_PAB
             WHERE
                   CAGPTO_CTA         = :GFCTB0I1.CAGPTO-CTA         AND
                 ((CDEPDC             = :GFCTB0I1.CDEPDC             AND
                   CPOSTO_SERVC       = :GFCTB0I1.CPOSTO-SERVC       AND
                   CSERVC_TARIF       = :GFCTB0I1.CSERVC-TARIF       AND
                   DINIC_SUSP_COBR   >= :GFCTB0I1.DINIC-SUSP-COBR)   OR
                  (CDEPDC             = :GFCTB0I1.CDEPDC             AND
                   CPOSTO_SERVC       = :GFCTB0I1.CPOSTO-SERVC       AND
                   CSERVC_TARIF      >= :WRK-CSERVC-TARIF-INI        AND
                   CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM)       OR
                  (CDEPDC             = :GFCTB0I1.CDEPDC             AND
                   CPOSTO_SERVC      >  :GFCTB0I1.CPOSTO-SERVC       AND
                   CSERVC_TARIF      >= :WRK-CSERVC-TARIF-INI        AND
                   CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM)       OR
                  (CDEPDC            >  :GFCTB0I1.CDEPDC             AND
                   CSERVC_TARIF      >= :WRK-CSERVC-TARIF-FILTRO     AND
                   CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM))
               ORDER BY
                   CDEPDC,
                   CPOSTO_SERVC,
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR
           END-EXEC.

           EXEC SQL
             DECLARE   CSR02-GFCTB0E4   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   CDEPDC
             FROM  DB2PRD.SUSP_GRP_DEPDC
             WHERE
                   CAGPTO_CTA         = :GFCTB0E4.CAGPTO-CTA         AND
                 ((CDEPDC             = :GFCTB0E4.CDEPDC             AND
                   CSERVC_TARIF       = :GFCTB0E4.CSERVC-TARIF       AND
                   DINIC_SUSP_COBR   >= :GFCTB0E4.DINIC-SUSP-COBR)   OR
                  (CDEPDC             = :GFCTB0E4.CDEPDC             AND
                   CSERVC_TARIF      >= :WRK-CSERVC-TARIF-INI        AND
                   CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM)       OR
                  (CDEPDC            >  :GFCTB0E4.CDEPDC             AND
                   CSERVC_TARIF      >= :WRK-CSERVC-TARIF-FILTRO     AND
                   CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM))
           ORDER BY
                   CDEPDC,
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR
           END-EXEC.

           EXEC SQL
             DECLARE   CSR03-GFCTB0E3   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
ST25X6             CCGC_CPF_ST,
ST25X6             CFLIAL_CGC_ST,
ST25X6             CCTRL_CPF_CGC_ST
             FROM  DB2PRD.SUSP_GRP_CLI
             WHERE
                   CAGPTO_CTA         = :GFCTB0E3.CAGPTO-CTA         AND
ST25X6*          ((CCGC_CPF           = :GFCTB0E3.CCGC-CPF           AND
ST25X6*            CSERVC_TARIF       = :GFCTB0E3.CSERVC-TARIF       AND
ST25X6*            DINIC_SUSP_COBR   >= :GFCTB0E3.DINIC-SUSP-COBR)   OR
ST25X6*           (CCGC_CPF           = :GFCTB0E3.CCGC-CPF           AND
ST25X6*            CSERVC_TARIF      >= :WRK-CSERVC-TARIF-INI        AND
ST25X6*            CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM)       OR
ST25X6*           (CCGC_CPF          > :GFCTB0E3.CCGC-CPF            AND
ST25X6*            CSERVC_TARIF      >= :WRK-CSERVC-TARIF-FILTRO     AND
ST25X6*            CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM))
ST25X6           ((CCGC_CPF_ST        = :GFCTB0E3.CCGC-CPF-ST        AND
ST25X6             CSERVC_TARIF       = :GFCTB0E3.CSERVC-TARIF       AND
ST25X6             DINIC_SUSP_COBR   >= :GFCTB0E3.DINIC-SUSP-COBR)   OR
ST25X6            (CCGC_CPF_ST        = :GFCTB0E3.CCGC-CPF-ST        AND
ST25X6             CSERVC_TARIF      >= :WRK-CSERVC-TARIF-INI        AND
ST25X6             CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM)       OR
ST25X6            (CCGC_CPF_ST       > :GFCTB0E3.CCGC-CPF-ST         AND
ST25X6             CSERVC_TARIF      >= :WRK-CSERVC-TARIF-FILTRO     AND
ST25X6             CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM))
             ORDER BY
ST25X6*            CCGC_CPF,
ST25X6             CCGC_CPF_ST,
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR
           END-EXEC.

           EXEC SQL
             DECLARE   CSR04-GFCTB0E5   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_SUSP_COBR,
                   CSEQ_AGPTO_CTA,
                   CMUN_IBGE
             FROM  DB2PRD.SUSP_GRP_MUN
             WHERE
                   CAGPTO_CTA         = :GFCTB0E5.CAGPTO-CTA         AND
                 ((CMUN_IBGE          = :GFCTB0E5.CMUN-IBGE          AND
                   CSERVC_TARIF       = :GFCTB0E5.CSERVC-TARIF       AND
                   DINIC_SUSP_COBR   >= :GFCTB0E5.DINIC-SUSP-COBR)   OR
                  (CMUN_IBGE          = :GFCTB0E5.CMUN-IBGE          AND
                   CSERVC_TARIF      >= :WRK-CSERVC-TARIF-INI        AND
                   CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM)       OR
                  (CMUN_IBGE         >  :GFCTB0E5.CMUN-IBGE          AND
                   CSERVC_TARIF      >= :WRK-CSERVC-TARIF-FILTRO     AND
                   CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM))
             ORDER BY
                   CMUN_IBGE,
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR
           END-EXEC.

           EXEC SQL
             DECLARE   CSR05-GFCTB0E6   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_SUSP_COBR,
                   CSEQ_AGPTO_CTA,
                   CSGL_UF
             FROM  DB2PRD.SUSP_GRP_UF
             WHERE
                   CAGPTO_CTA         = :GFCTB0E6.CAGPTO-CTA         AND
                 ((CSGL_UF            = :GFCTB0E6.CSGL-UF            AND
                   CSERVC_TARIF       = :GFCTB0E6.CSERVC-TARIF       AND
                   DINIC_SUSP_COBR   >= :GFCTB0E6.DINIC-SUSP-COBR)   OR
                  (CSGL_UF            = :GFCTB0E6.CSGL-UF            AND
                   CSERVC_TARIF      >= :WRK-CSERVC-TARIF-INI        AND
                   CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM)       OR
                  (CSGL_UF           >  :GFCTB0E6.CSGL-UF            AND
                   CSERVC_TARIF      >= :WRK-CSERVC-TARIF-FILTRO     AND
                   CSERVC_TARIF      <= :WRK-CSERVC-TARIF-FIM))
             ORDER BY
                   CSGL_UF,
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTPA'.

       COPY 'I#GFCTPB'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTPA-ENTRADA
                                                      GFCTPB-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       00000-INICIAR                   SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIALIZAR.

           PERFORM 20000-PROCESSAR.

           PERFORM 30000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCEDIMENTOS INICIAIS DO PROGRAMA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTPB-SAIDA.

           INITIALIZE GFCTPB-SAIDA
                      GFCT0M-AREA-ERROS
                      IND-1.

           PERFORM 11000-VERIFICAR-DISP-SISTEMA.

           PERFORM 12000-MONTAR-AREA-SAIDA.

           PERFORM 13000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       11000-VERIFICAR-DISP-SISTEMA    SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTPA-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTPA-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTPA-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTPB-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0292'         TO GFCT0M-TRANSACAO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 9                  TO GFCTPB-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0292'     TO GFCT0M-TRANSACAO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTPB-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0292'             TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA AREA DE RETORNO AO CHAMADOR                             *
      ******************************************************************
      *----------------------------------------------------------------*
       12000-MONTAR-AREA-SAIDA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTPB-SAIDA.

           MOVE 310                        TO GFCTPB-LL.
           MOVE ZEROS                      TO GFCTPB-ZZ.
           MOVE GFCTPA-TRANSACAO           TO GFCTPB-TRANSACAO.
           MOVE GFCTPA-FUNCAO              TO GFCTPB-FUNCAO.

           IF  GFCTPA-QTDE-OCOR            NOT NUMERIC
               MOVE ZEROS                  TO GFCTPB-QTDE-OCOR
           ELSE
               MOVE GFCTPA-QTDE-OCOR       TO GFCTPB-QTDE-OCOR
           END-IF.

           IF  GFCTPA-QTDE-TOT-REG         NOT NUMERIC
               MOVE ZEROS                  TO GFCTPB-QTDE-TOT-REG
           ELSE
               MOVE GFCTPA-QTDE-TOT-REG    TO GFCTPB-QTDE-TOT-REG
           END-IF.

           MOVE GFCTPA-FUNC-BDSCO          TO GFCTPB-FUNC-BDSCO.
           MOVE GFCTPA-PONTEIRO            TO GFCTPB-PONTEIRO.
           MOVE GFCTPA-FILTRO-TARIFA       TO GFCTPB-FILTRO-TARIFA.

           MOVE 'N'                        TO GFCTPB-FIM.

           MOVE ZEROS                      TO GFCTPB-ERRO
                                              GFCTPB-COD-SQL-ERRO
                                              GFCTPB-COD-MSG-ERRO.
           MOVE '0000'                     TO GFCTG2-COD-MSG.

           PERFORM 12100-OBTER-DESC-MSG.

           MOVE  GFCTG3-DESC-MSG           TO GFCTPB-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO GFCT5523 PARA OBTER A DESCRICAO DA MENSAGEM       *
      ******************************************************************
      *----------------------------------------------------------------*
       12100-OBTER-DESC-MSG            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTPA-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTPA-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTPA-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTPB-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0292'         TO GFCT0M-TRANSACAO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTPB-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0292'     TO GFCT0M-TRANSACAO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 'S'                TO GFCTPB-FIM
               MOVE 1                  TO GFCTPB-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTPB-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTPB-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       12100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       13000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTPA-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTPA-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTPA-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTPA-QTDE-OCOR        NOT NUMERIC)                OR
              (GFCTPA-QTDE-TOT-REG     NOT NUMERIC)                OR
              (GFCTPA-FILTRO-TARIFA    NOT NUMERIC)                OR
              (GFCTPA-PONT-AGPTO       NOT NUMERIC)                OR
              (GFCTPA-PONT-AGPTO       NOT EQUAL 4 AND 14 AND 15   AND
                                                       18 AND 19)  OR
              (GFCTPA-PONT-TARIFA      NOT NUMERIC)                OR
              (GFCTPA-PONT-CH-NUM5     NOT NUMERIC)                OR
              (GFCTPA-PONT-CH-NUM13    NOT NUMERIC)
               MOVE  1                 TO GFCTPB-ERRO
               MOVE  ZEROS             TO GFCTPB-COD-SQL-ERRO
               MOVE  0001              TO GFCTPB-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTPB-FIM
               PERFORM 12100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       13000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-OBTER-DATA-PROCM.

           PERFORM 22000-OBTER-COD-JUNCAO.

           PERFORM 23000-VERIFICA-MASTER.

           PERFORM 24000-TRATAR-CURSORES.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA CONSULTA NA TABELA GFCTB0A1                              *
      ******************************************************************
      *----------------------------------------------------------------*
       21000-OBTER-DATA-PROCM          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_PROX,
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-PROX,
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPB-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1(1:2)
                                       TO WRK-DPROCM-ATUAL-DIA.
           MOVE DPROCM-ATUAL OF GFCTB0A1(4:2)
                                       TO WRK-DPROCM-ATUAL-MES.
           MOVE DPROCM-ATUAL OF GFCTB0A1(7:4)
                                       TO WRK-DPROCM-ATUAL-ANO.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA AREA DE ERRO EM ACESSO A TABELAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       21100-FORMATAR-ERRO-DB2         SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTPB-ERRO.
           MOVE 'GFCT0292'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTPB-COD-SQL-ERRO.

           PERFORM 12100-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTPB-DESC-MSG-ERRO.

           PERFORM 30000-FINALIZAR.

      *----------------------------------------------------------------*
       21100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA O MODULO SENH0315                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       22000-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCTPA-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 'S'                TO GFCTPB-FIM
               MOVE 1                  TO GFCTPB-ERRO
               MOVE RETURN-CODE        TO GFCTPB-COD-SQL-ERRO
               MOVE 0056               TO GFCTPB-COD-MSG-ERRO
               MOVE '0056'             TO GFCTPB-COD-MSG-ERRO
               PERFORM 12100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MENSAGEM-0315   EQUAL 001
                   MOVE 'S'                TO GFCTPB-FIM
                   MOVE 1                  TO GFCTPB-ERRO
                   MOVE RETURN-CODE        TO GFCTPB-COD-SQL-ERRO
                   MOVE 0057               TO GFCTPB-COD-MSG-ERRO
                   MOVE '0057'             TO GFCTG2-COD-MSG
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 154
                   MOVE 'S'                TO GFCTPB-FIM
                   MOVE 1                  TO GFCTPB-ERRO
                   MOVE RETURN-CODE        TO GFCTPB-COD-SQL-ERRO
                   MOVE 0058               TO GFCTPB-COD-MSG-ERRO
                   MOVE '0058'             TO GFCTG2-COD-MSG
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 242
                   MOVE 'S'                TO GFCTPB-FIM
                   MOVE 1                  TO GFCTPB-ERRO
                   MOVE RETURN-CODE        TO GFCTPB-COD-SQL-ERRO
                   MOVE 0059               TO GFCTPB-COD-MSG-ERRO
                   MOVE '0059'             TO GFCTG2-COD-MSG
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 009
                   MOVE 'S'                TO GFCTPB-FIM
                   MOVE 1                  TO GFCTPB-ERRO
                                              GFCTPB-COD-MSG-ERRO
                   MOVE RETURN-CODE        TO GFCTPB-COD-SQL-ERRO
                   MOVE '0001'             TO GFCTG2-COD-MSG
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315   EQUAL 138
                   MOVE 'S'                TO GFCTPB-FIM
                   MOVE 1                  TO GFCTPB-ERRO
                   MOVE RETURN-CODE        TO GFCTPB-COD-SQL-ERRO
                   MOVE 0060               TO GFCTPB-COD-MSG-ERRO
                   MOVE '0060'             TO GFCTG2-COD-MSG
                   PERFORM 12100-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA O MODULO GFCT5507                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       23000-VERIFICA-MASTER           SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5507-AREA.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO GFCTPB-ERRO
               MOVE 'GFCT0292'             TO GFCT0M-PROGRAMA
               MOVE 'GFCT5507'             TO WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  WRK-5507-COD-RETORNO        NOT EQUAL ZEROS
               IF  WRK-5507-COD-RETORNO    EQUAL 01
                   MOVE 1                  TO GFCTPB-ERRO
                   MOVE ZEROS              TO GFCTPB-COD-SQL-ERRO
                                              GFCTPB-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                           TO GFCTPB-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO    EQUAL 99
                       MOVE 1              TO GFCTPB-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO TO GFCTPB-COD-SQL-ERRO
                       MOVE ZEROS          TO GFCTPB-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                           TO GFCTPB-DESC-MSG-ERRO
                   END-IF
               END-IF
               MOVE 'S'                    TO GFCTPB-FIM
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSORES                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24000-TRATAR-CURSORES           SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTPA-PONT-AGPTO

               WHEN 15
                   PERFORM 24100-CONSULTAR-PAB

               WHEN 14
                   PERFORM 24200-CONSULTAR-AGENCIA

               WHEN 4
                   PERFORM 24300-CONSULTAR-CLIENTE

               WHEN 18
                   PERFORM 24400-CONSULTAR-MUNICIPIO

               WHEN 19
                   PERFORM 24500-CONSULTAR-UF

           END-EVALUATE.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 1                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24100-CONSULTAR-PAB             SECTION.
      *----------------------------------------------------------------*

           PERFORM 24110-ABRIR-CSR01.

           PERFORM 24120-LER-CSR01.

           PERFORM 24130-PROCESSAR-CSR01   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 24140-FECHAR-CSR01.

      *----------------------------------------------------------------*
       24100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 1                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24110-ABRIR-CSR01               SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE GFCTPA-PONT-AGPTO      TO CAGPTO-CTA       OF GFCTB0I1.

           IF GFCTPA-FILTRO-TARIFA     EQUAL ZEROS
              MOVE GFCTPA-FILTRO-TARIFA
                                       TO WRK-CSERVC-TARIF-FILTRO
              MOVE GFCTPA-PONT-TARIFA  TO WRK-CSERVC-TARIF-INI
                                          CSERVC-TARIF     OF GFCTB0I1
              MOVE 99999               TO WRK-CSERVC-TARIF-FIM
           ELSE
              MOVE GFCTPA-FILTRO-TARIFA
                                       TO WRK-CSERVC-TARIF-FILTRO
                                          WRK-CSERVC-TARIF-INI
                                          CSERVC-TARIF     OF GFCTB0I1
                                          WRK-CSERVC-TARIF-FIM
           END-IF.

           MOVE GFCTPA-PONT-CH-NUM5    TO CDEPDC           OF GFCTB0I1.
           MOVE GFCTPA-PONT-CH-NUM13   TO CPOSTO-SERVC     OF GFCTB0I1.
           MOVE 'N'                    TO WRK-FINALIZAR.

           IF  GFCTPA-PONT-CH-DATA     EQUAL SPACES OR LOW-VALUES
               MOVE '01.01.0001'         TO DINIC-SUSP-COBR  OF GFCTB0I1
           ELSE
               MOVE GFCTPA-PONT-CH-DATA  TO DINIC-SUSP-COBR  OF GFCTB0I1
           END-IF.

           EXEC SQL
               OPEN CSR01-GFCTB0I1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TSUSP_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTPB-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0I1'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 1                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       24120-LER-CSR01                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR01-GFCTB0I1   INTO
                   :GFCTB0I1.CSERVC-TARIF,
                   :GFCTB0I1.CAGPTO-CTA,
                   :GFCTB0I1.DINIC-SUSP-COBR,
                   :GFCTB0I1.CSEQ-AGPTO-CTA,
                   :GFCTB0I1.CDEPDC,
                   :GFCTB0I1.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TSUSP_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTPB-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0I1'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTPB-FIM
               PERFORM 24121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       24120-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA SE HA REGISTROS PARA SEREM PROCESSADOS PELO CURSOR     *
      ******************************************************************
      *----------------------------------------------------------------*
       24121-VER-VAZIO                 SECTION.
      *----------------------------------------------------------------*

           IF  IND-1                   EQUAL 1
               MOVE 1                  TO GFCTPB-ERRO
               MOVE ZEROS              TO GFCTPB-COD-SQL-ERRO
               MOVE 0908               TO GFCTPB-COD-MSG-ERRO
               MOVE '0908'             TO GFCTG2-COD-MSG
               PERFORM 12100-OBTER-DESC-MSG
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
               DELIMITED BY '  '       INTO GFCTPB-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       24121-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 1                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       24130-PROCESSAR-CSR01           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0I1
                                       TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE CAGPTO-CTA             OF GFCTB0I1
                                       TO CAGPTO-CTA       OF GFCTB0E7.
           MOVE DINIC-SUSP-COBR        OF GFCTB0I1
                                       TO DINIC-SUSP-COBR  OF GFCTB0E7.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0I1
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0E7.

           PERFORM 24131-ACESSAR-GFCTB0E7.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 24120-LER-CSR01
                   GO TO 24130-99-FIM
               END-IF
           END-IF.

           IF  IND-1                   GREATER 5
               MOVE CSERVC-TARIF       OF GFCTB0I1
                                       TO GFCTPB-PONT-TARIFA
               MOVE CAGPTO-CTA         OF GFCTB0I1
                                       TO GFCTPB-PONT-AGPTO
               MOVE CDEPDC             OF GFCTB0I1
                                       TO GFCTPB-PONT-CH-NUM5
               MOVE CPOSTO-SERVC       OF GFCTB0I1
                                       TO GFCTPB-PONT-CH-NUM13
               MOVE DINIC-SUSP-COBR    OF GFCTB0I1
                                       TO GFCTPB-PONT-CH-DATA
               MOVE 'S'                TO WRK-FINALIZAR
               GO TO 24130-99-FIM
           END-IF.

           MOVE DINIC-SUSP-COBR        OF GFCTB0I1
                                       TO GFCTPB-DATA-INICIO(IND-1).
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0I1
                                       TO GFCTPB-SEQUENCIA(IND-1).
           MOVE CDEPDC                 OF GFCTB0I1
                                       TO GFCTPB-AGENCIA(IND-1).
           MOVE CPOSTO-SERVC           OF GFCTB0I1
                                       TO GFCTPB-POSTO(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB0I1
                                       TO GFCTPB-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8 TO W-DE20

           MOVE W-DE20                 TO GFCTPB-DESC-TARIFA(IND-1).

           MOVE DFIM-SUSP-COBR OF GFCTB0E7(1:2)
                                       TO WRK-DFIM-SUSP-COBR-DIA.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7(4:2)
                                       TO WRK-DFIM-SUSP-COBR-MES.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7(7:4)
                                       TO WRK-DFIM-SUSP-COBR-ANO.

           IF WRK-DFIM-SUSP-COBR       NOT LESS WRK-DPROCM-ATUAL
               MOVE 'S'                TO GFCTPB-ATIVO(IND-1)
           ELSE
               MOVE 'N'                TO GFCTPB-ATIVO(IND-1)
           END-IF.

           IF  CINDCD-AGPTO-TOT        OF GFCTB0E7
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTPB-DESC-NOME(IND-1)
           ELSE
               PERFORM 24132-OBTER-DESC-PAB
               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTPB-DESC-NOME(IND-1)
           END-IF.

           ADD 140                     TO GFCTPB-LL.
           ADD 1                       TO GFCTPB-QTDE-TOT-REG
                                          IND-1.

           PERFORM 24120-LER-CSR01.

      *----------------------------------------------------------------*
       24130-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTA TABELAS GFCTB0E7 E GFCTB0D8                            *
      ******************************************************************
      *---------------------- -----------------------------------------*
       24131-ACESSAR-GFCTB0E7          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
                   A.DINIC_SUSP_COBR,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.CFUNC_MANUT,
                   A.CFUNC_MANUT_INCL,
                   A.DFIM_SUSP_COBR,
                   A.CINDCD_LIBRC_COBR,
                   A.DCOBR_SUSP_LIBRD,
                   A.RJUSTF_SUSP_COBR,
                   A.CINDCD_AGPTO_TOT,
                   A.HMANUT_REG,
                   B.CDEPDC,
                   B.RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0E7.CSERVC-TARIF,
                   :GFCTB0E7.DINIC-SUSP-COBR,
                   :GFCTB0E7.CAGPTO-CTA,
                   :GFCTB0E7.CSEQ-AGPTO-CTA,
                   :GFCTB0E7.CFUNC-MANUT,
                   :GFCTB0E7.CFUNC-MANUT-INCL,
                   :GFCTB0E7.DFIM-SUSP-COBR,
                   :GFCTB0E7.CINDCD-LIBRC-COBR,
                   :GFCTB0E7.DCOBR-SUSP-LIBRD,
                   :GFCTB0E7.RJUSTF-SUSP-COBR,
                   :GFCTB0E7.CINDCD-AGPTO-TOT,
                   :GFCTB0E7.HMANUT-REG,
                   :GFCTB0D8.CDEPDC,
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SUSP_TARIF_AGPTO A,
                    DB2PRD.SERVC_TARIF_PRINC B
             WHERE
                   A.CSERVC_TARIF     = B.CSERVC_TARIF               AND
                   A.CSERVC_TARIF     = :GFCTB0E7.CSERVC-TARIF       AND
                   A.CAGPTO_CTA       = :GFCTB0E7.CAGPTO-CTA         AND
                   A.CSEQ_AGPTO_CTA   = :GFCTB0E7.CSEQ-AGPTO-CTA     AND
                   A.DINIC_SUSP_COBR  = :GFCTB0E7.DINIC-SUSP-COBR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPB-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0I1'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24131-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO GFCT5109                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       24132-OBTER-DESC-PAB            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTGB-ENTRADA
                      GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTPA-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTPA-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR
                                          GFCTGB-QTDE-TOT-REG.
           MOVE GFCTPA-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC                 OF GFCTB0I1
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB0I1
                                       TO GFCTGB-COD-POSTO.
           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTPB-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0292'         TO GFCT0M-TRANSACAO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTPB-FIM
               MOVE GFCTGC-ERRO        TO GFCTPB-ERRO
               MOVE GFCTGC-COD-SQL-ERRO    TO GFCTPB-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO    TO GFCTPB-COD-MSG-ERRO
               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTPB-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109 - ' GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTPB-DESC-MSG-ERRO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24132-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 1                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       24140-FECHAR-CSR01              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0I1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TSUSP_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTPB-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0I1'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24140-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 2                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24200-CONSULTAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           PERFORM 24210-ABRIR-CSR02.

           PERFORM 24220-LER-CSR02.

           PERFORM 24230-PROCESSAR-CSR02   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 24240-FECHAR-CSR02.

      *----------------------------------------------------------------*
       24200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 2                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24210-ABRIR-CSR02               SECTION.
      *----------------------------------------------------------------*

           MOVE 1                       TO IND-1.
           MOVE GFCTPA-PONT-AGPTO       TO CAGPTO-CTA       OF GFCTB0E4.

           IF GFCTPA-FILTRO-TARIFA     EQUAL ZEROS
              MOVE GFCTPA-FILTRO-TARIFA
                                       TO WRK-CSERVC-TARIF-FILTRO
              MOVE GFCTPA-PONT-TARIFA  TO WRK-CSERVC-TARIF-INI
                                          CSERVC-TARIF     OF GFCTB0E4
              MOVE 99999               TO WRK-CSERVC-TARIF-FIM
           ELSE
              MOVE GFCTPA-FILTRO-TARIFA
                                       TO WRK-CSERVC-TARIF-FILTRO
                                          WRK-CSERVC-TARIF-INI
                                          CSERVC-TARIF     OF GFCTB0E4
                                          WRK-CSERVC-TARIF-FIM
           END-IF.

           MOVE GFCTPA-PONT-CH-NUM5     TO CDEPDC           OF GFCTB0E4.
           MOVE 'N'                     TO WRK-FINALIZAR.

           IF  GFCTPA-PONT-CH-DATA      EQUAL SPACES
               MOVE '01.01.0001'        TO DINIC-SUSP-COBR  OF GFCTB0E4
           ELSE
               MOVE GFCTPA-PONT-CH-DATA TO DINIC-SUSP-COBR  OF GFCTB0E4
           END-IF.

           EXEC SQL
               OPEN CSR02-GFCTB0E4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_DEPDC  ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTPB-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E4'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24210-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 2                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       24220-LER-CSR02                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR02-GFCTB0E4   INTO
                   :GFCTB0E4.CSERVC-TARIF,
                   :GFCTB0E4.DINIC-SUSP-COBR,
                   :GFCTB0E4.CAGPTO-CTA,
                   :GFCTB0E4.CSEQ-AGPTO-CTA,
                   :GFCTB0E4.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_DEPDC'   TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTPB-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E4'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTPB-FIM
               PERFORM 24121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       24220-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 2                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       24230-PROCESSAR-CSR02           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E4
                                       TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE CAGPTO-CTA             OF GFCTB0E4
                                       TO CAGPTO-CTA       OF GFCTB0E7.
           MOVE DINIC-SUSP-COBR        OF GFCTB0E4
                                       TO DINIC-SUSP-COBR  OF GFCTB0E7.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E4
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0E7.

           PERFORM 24131-ACESSAR-GFCTB0E7.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 24220-LER-CSR02
                   GO TO 24230-99-FIM
               END-IF
           END-IF.

           IF  IND-1                   GREATER 5
               MOVE CSERVC-TARIF       OF GFCTB0E4
                                       TO GFCTPB-PONT-TARIFA
               MOVE CAGPTO-CTA         OF GFCTB0E4
                                       TO GFCTPB-PONT-AGPTO
               MOVE CDEPDC             OF GFCTB0E4
                                       TO GFCTPB-PONT-CH-NUM5
               MOVE DINIC-SUSP-COBR    OF GFCTB0E4
                                       TO GFCTPB-PONT-CH-DATA
               MOVE 'S'                TO WRK-FINALIZAR
               GO TO 24230-99-FIM
           END-IF.

           MOVE DINIC-SUSP-COBR        OF GFCTB0E4
                                       TO GFCTPB-DATA-INICIO(IND-1).
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E4
                                       TO GFCTPB-SEQUENCIA(IND-1).
           MOVE CDEPDC                 OF GFCTB0E4
                                       TO GFCTPB-AGENCIA(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB0E4
                                       TO GFCTPB-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8 TO W-DE20

           MOVE W-DE20                 TO GFCTPB-DESC-TARIFA(IND-1).

           MOVE DFIM-SUSP-COBR OF GFCTB0E7(1:2)
                                       TO WRK-DFIM-SUSP-COBR-DIA.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7(4:2)
                                       TO WRK-DFIM-SUSP-COBR-MES.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7(7:4)
                                       TO WRK-DFIM-SUSP-COBR-ANO.

           IF WRK-DFIM-SUSP-COBR       NOT LESS WRK-DPROCM-ATUAL
               MOVE 'S'                TO GFCTPB-ATIVO(IND-1)
           ELSE
               MOVE 'N'                TO GFCTPB-ATIVO(IND-1)
           END-IF.

           IF  CINDCD-AGPTO-TOT        OF GFCTB0E7
                                       EQUAL 'S'
               MOVE 'TODAS'            TO GFCTPB-DESC-NOME(IND-1)
           ELSE
               PERFORM 24231-OBTER-DESC-DEPDC
               MOVE GFCT1X-DESC-DEPDC(1)
                                       TO GFCTPB-DESC-NOME(IND-1)
           END-IF.

           ADD 140                     TO GFCTPB-LL.
           ADD 1                       TO GFCTPB-QTDE-TOT-REG
                                          IND-1.

           PERFORM 24220-LER-CSR02.

      *----------------------------------------------------------------*
       24230-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO GFCT5006                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       24231-OBTER-DESC-DEPDC          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCT1V-ENTRADA
                      GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTPA-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTPA-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTPA-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE CDEPDC                 OF GFCTB0E4
                                       TO GFCT1V-COD-DEPDC(1).
           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTPB-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0292'         TO GFCT0M-TRANSACAO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTPB-FIM
               MOVE GFCT1X-ERRO        TO GFCTPB-ERRO
               MOVE GFCT1X-COD-SQL-ERRO    TO GFCTPB-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO    TO GFCTPB-COD-MSG-ERRO
               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTPB-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006 - ' GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTPB-DESC-MSG-ERRO
               END-IF
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24231-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 2                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       24240-FECHAR-CSR02              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR02-GFCTB0E4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_DEPDC'   TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTPB-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E4'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24240-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 3                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24300-CONSULTAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           PERFORM 24310-ABRIR-CSR03.

           PERFORM 24320-LER-CSR03.

           PERFORM 24330-PROCESSAR-CSR03   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 24340-FECHAR-CSR03.

      *----------------------------------------------------------------*
       24300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 3                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24310-ABRIR-CSR03               SECTION.
      *----------------------------------------------------------------*

           MOVE 1                       TO IND-1.
           MOVE GFCTPA-PONT-AGPTO       TO CAGPTO-CTA       OF GFCTB0E3.

           IF GFCTPA-FILTRO-TARIFA     EQUAL ZEROS
              MOVE GFCTPA-FILTRO-TARIFA
                                       TO WRK-CSERVC-TARIF-FILTRO
              MOVE GFCTPA-PONT-TARIFA  TO WRK-CSERVC-TARIF-INI
                                          CSERVC-TARIF     OF GFCTB0E3
              MOVE 99999               TO WRK-CSERVC-TARIF-FIM
           ELSE
              MOVE GFCTPA-FILTRO-TARIFA
                                       TO WRK-CSERVC-TARIF-FILTRO
                                          WRK-CSERVC-TARIF-INI
                                          CSERVC-TARIF     OF GFCTB0E3
                                          WRK-CSERVC-TARIF-FIM
           END-IF.

ST25X6*    MOVE GFCTPA-PONT-CH-NUM13    TO CCGC-CPF         OF GFCTB0E3.
ST25X6     MOVE GFCTPA-PONT-CH-NUM13    TO CCGC-CPF-ST      OF GFCTB0E3.
           MOVE 'N'                     TO WRK-FINALIZAR.

           IF  GFCTPA-PONT-CH-DATA      EQUAL SPACES
               MOVE '01.01.0001'        TO DINIC-SUSP-COBR  OF GFCTB0E3
           ELSE
               MOVE GFCTPA-PONT-CH-DATA TO DINIC-SUSP-COBR  OF GFCTB0E3
           END-IF.

           EXEC SQL
               OPEN CSR03-GFCTB0E3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_CLI'     TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTPB-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E3'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 3                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       24320-LER-CSR03                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR03-GFCTB0E3   INTO
                   :GFCTB0E3.CSERVC-TARIF,
                   :GFCTB0E3.DINIC-SUSP-COBR,
                   :GFCTB0E3.CAGPTO-CTA,
                   :GFCTB0E3.CSEQ-AGPTO-CTA,
                   :GFCTB0E3.CCGC-CPF,
                   :GFCTB0E3.CFLIAL-CGC,
                   :GFCTB0E3.CCTRL-CPF-CGC,
ST25X6             :GFCTB0E3.CCGC-CPF-ST,
ST25X6             :GFCTB0E3.CFLIAL-CGC-ST,
ST25X6             :GFCTB0E3.CCTRL-CPF-CGC-ST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_CLI'     TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTPB-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E3'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTPB-FIM
               PERFORM 24121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       24320-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 3                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       24330-PROCESSAR-CSR03           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E3
                                       TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE CAGPTO-CTA             OF GFCTB0E3
                                       TO CAGPTO-CTA       OF GFCTB0E7.
           MOVE DINIC-SUSP-COBR        OF GFCTB0E3
                                       TO DINIC-SUSP-COBR  OF GFCTB0E7.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E3
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0E7.

           PERFORM 24131-ACESSAR-GFCTB0E7.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 24320-LER-CSR03
                   GO TO 24330-99-FIM
               END-IF
           END-IF.

           IF  IND-1                   GREATER 5
               MOVE CSERVC-TARIF       OF GFCTB0E3
                                       TO GFCTPB-PONT-TARIFA
               MOVE CAGPTO-CTA         OF GFCTB0E3
                                       TO GFCTPB-PONT-AGPTO
ST25X6*        MOVE CCGC-CPF           OF GFCTB0E3
ST25X6*                                TO GFCTPB-PONT-CH-NUM13
ST25X6         MOVE CCGC-CPF-ST        OF GFCTB0E3
ST25X6                                 TO GFCTPB-PONT-CH-NUM13
               MOVE DINIC-SUSP-COBR    OF GFCTB0E3
                                       TO GFCTPB-PONT-CH-DATA
               MOVE 'S'                TO WRK-FINALIZAR
               GO TO 24330-99-FIM
           END-IF.

           MOVE DINIC-SUSP-COBR        OF GFCTB0E3
                                       TO GFCTPB-DATA-INICIO(IND-1).
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E3
                                       TO GFCTPB-SEQUENCIA(IND-1).
ST25X6*    MOVE CCGC-CPF               OF GFCTB0E3
ST25X6*                                TO GFCTPB-CPF-CNPJ(IND-1).
ST25X6     MOVE CCGC-CPF-ST            OF GFCTB0E3
ST25X6                                 TO GFCTPB-CPF-CNPJ(IND-1).
ST25X6*    MOVE CFLIAL-CGC             OF GFCTB0E3
ST25X6*                                TO GFCTPB-FILIAL(IND-1).
ST25X6     MOVE CFLIAL-CGC-ST          OF GFCTB0E3
ST25X6                                 TO GFCTPB-FILIAL(IND-1).
ST25X6*    MOVE CCTRL-CPF-CGC          OF GFCTB0E3
ST25X6*                                TO GFCTPB-CONTROLE(IND-1).
ST25X6     MOVE CCTRL-CPF-CGC-ST       OF GFCTB0E3
ST25X6                                 TO GFCTPB-CONTROLE(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB0E3
                                       TO GFCTPB-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8 TO W-DE20

           MOVE W-DE20                 TO GFCTPB-DESC-TARIFA(IND-1).

           MOVE DFIM-SUSP-COBR OF GFCTB0E7(1:2)
                                       TO WRK-DFIM-SUSP-COBR-DIA.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7(4:2)
                                       TO WRK-DFIM-SUSP-COBR-MES.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7(7:4)
                                       TO WRK-DFIM-SUSP-COBR-ANO.

           IF WRK-DFIM-SUSP-COBR       NOT LESS WRK-DPROCM-ATUAL
               MOVE 'S'                TO GFCTPB-ATIVO(IND-1)
           ELSE
               MOVE 'N'                TO GFCTPB-ATIVO(IND-1)
           END-IF.

           ADD 140                     TO GFCTPB-LL.
           ADD 1                       TO GFCTPB-QTDE-TOT-REG
                                          IND-1.

           PERFORM 24320-LER-CSR03.

      *----------------------------------------------------------------*
       24330-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 3                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       24340-FECHAR-CSR03              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR03-GFCTB0E3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_CLI'      TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTPB-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E3'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24340-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 4                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24400-CONSULTAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           PERFORM 24410-ABRIR-CSR04.

           PERFORM 24420-LER-CSR04.

           PERFORM 24430-PROCESSAR-CSR04   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 24440-FECHAR-CSR04.

      *----------------------------------------------------------------*
       24400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 4                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24410-ABRIR-CSR04               SECTION.
      *----------------------------------------------------------------*

           MOVE 1                       TO IND-1.
           MOVE GFCTPA-PONT-AGPTO       TO CAGPTO-CTA       OF GFCTB0E5.

           IF GFCTPA-FILTRO-TARIFA     EQUAL ZEROS
              MOVE GFCTPA-FILTRO-TARIFA
                                       TO WRK-CSERVC-TARIF-FILTRO
              MOVE GFCTPA-PONT-TARIFA  TO WRK-CSERVC-TARIF-INI
                                          CSERVC-TARIF     OF GFCTB0E5
              MOVE 99999               TO WRK-CSERVC-TARIF-FIM
           ELSE
              MOVE GFCTPA-FILTRO-TARIFA
                                       TO WRK-CSERVC-TARIF-FILTRO
                                          WRK-CSERVC-TARIF-INI
                                          CSERVC-TARIF     OF GFCTB0E5
                                          WRK-CSERVC-TARIF-FIM
           END-IF.

           MOVE GFCTPA-PONT-CH-NUM5     TO CMUN-IBGE        OF GFCTB0E5.
           MOVE 'N'                     TO WRK-FINALIZAR.

           IF  GFCTPA-PONT-CH-DATA      EQUAL SPACES OR LOW-VALUES
               MOVE '01.01.0001'        TO DINIC-SUSP-COBR  OF GFCTB0E5
           ELSE
               MOVE GFCTPA-PONT-CH-DATA TO DINIC-SUSP-COBR OF GFCTB0E5
           END-IF.

           EXEC SQL
               OPEN CSR04-GFCTB0E5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_MUN    ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTPB-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E5'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24410-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 4                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       24420-LER-CSR04                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR04-GFCTB0E5   INTO
                   :GFCTB0E5.CSERVC-TARIF,
                   :GFCTB0E5.CAGPTO-CTA,
                   :GFCTB0E5.DINIC-SUSP-COBR,
                   :GFCTB0E5.CSEQ-AGPTO-CTA,
                   :GFCTB0E5.CMUN-IBGE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_MUN'     TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTPB-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E5'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTPB-FIM
               PERFORM 24121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       24420-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 4                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       24430-PROCESSAR-CSR04           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E5
                                       TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE CAGPTO-CTA             OF GFCTB0E5
                                       TO CAGPTO-CTA       OF GFCTB0E7.
           MOVE DINIC-SUSP-COBR        OF GFCTB0E5
                                       TO DINIC-SUSP-COBR  OF GFCTB0E7.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E5
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0E7.

           PERFORM 24131-ACESSAR-GFCTB0E7.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 24420-LER-CSR04
                   GO TO 24430-99-FIM
               END-IF
           END-IF.

           IF  IND-1                   GREATER 5
               MOVE CSERVC-TARIF       OF GFCTB0E5
                                       TO GFCTPB-PONT-TARIFA
               MOVE CAGPTO-CTA         OF GFCTB0E5
                                       TO GFCTPB-PONT-AGPTO
               MOVE CMUN-IBGE          OF GFCTB0E5
                                       TO GFCTPB-PONT-CH-NUM5
               MOVE DINIC-SUSP-COBR    OF GFCTB0E5
                                       TO GFCTPB-PONT-CH-DATA
               MOVE 'S'                TO WRK-FINALIZAR
               GO TO 24430-99-FIM
           END-IF.

           MOVE DINIC-SUSP-COBR        OF GFCTB0E5
                                       TO GFCTPB-DATA-INICIO(IND-1).
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E5
                                       TO GFCTPB-SEQUENCIA(IND-1).
           MOVE CMUN-IBGE              OF GFCTB0E5
                                       TO GFCTPB-MUNICIPIO(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB0E5
                                       TO GFCTPB-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8 TO W-DE20
           MOVE W-DE20                 TO GFCTPB-DESC-TARIFA(IND-1).

           MOVE DFIM-SUSP-COBR OF GFCTB0E7(1:2)
                                       TO WRK-DFIM-SUSP-COBR-DIA.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7(4:2)
                                       TO WRK-DFIM-SUSP-COBR-MES.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7(7:4)
                                       TO WRK-DFIM-SUSP-COBR-ANO.

           IF WRK-DFIM-SUSP-COBR       NOT LESS WRK-DPROCM-ATUAL
               MOVE 'S'                TO GFCTPB-ATIVO(IND-1)
           ELSE
               MOVE 'N'                TO GFCTPB-ATIVO(IND-1)
           END-IF.

           PERFORM 24431-OBTER-DESC-MUNICIPIO.
           MOVE NOME-MUNIC-RURC88      TO GFCTPB-DESC-NOME(IND-1).

           ADD 140                     TO GFCTPB-LL.
           ADD 1                       TO GFCTPB-QTDE-TOT-REG
                                          IND-1.

           PERFORM 24420-LER-CSR04.

      *----------------------------------------------------------------*
       24430-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO RURC9020                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       24431-OBTER-DESC-MUNICIPIO      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE              OF GFCTB0E5
                                       TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTPB-FIM
               MOVE 1                  TO GFCTPB-ERRO
               MOVE ZEROS              TO GFCTPB-COD-SQL-ERRO
               MOVE  0732              TO GFCTPB-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 12100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 'S'                TO GFCTPB-FIM
               MOVE 1                  TO GFCTPB-ERRO
               MOVE ZEROS              TO GFCTPB-COD-SQL-ERRO
               MOVE  0675              TO GFCTPB-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 12100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 'S'                TO GFCTPB-FIM
               MOVE 1                  TO GFCTPB-ERRO
               MOVE ZEROS              TO GFCTPB-COD-SQL-ERRO
               MOVE  0733              TO GFCTPB-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 12100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 'S'                TO GFCTPB-FIM
               MOVE 1                  TO GFCTPB-ERRO
               MOVE ZEROS              TO GFCTPB-COD-SQL-ERRO
               MOVE  0734              TO GFCTPB-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 12100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24431-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 4                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       24440-FECHAR-CSR04              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR04-GFCTB0E5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_MUN'     TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTPB-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E5'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24440-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR 5                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24500-CONSULTAR-UF              SECTION.
      *----------------------------------------------------------------*

           PERFORM 24510-ABRIR-CSR05.

           PERFORM 24520-LER-CSR05.

           PERFORM 24530-PROCESSAR-CSR05   UNTIL
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 24540-FECHAR-CSR05.

      *----------------------------------------------------------------*
       24500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 5                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       24510-ABRIR-CSR05               SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.

           IF GFCTPA-FILTRO-TARIFA     EQUAL ZEROS
              MOVE GFCTPA-FILTRO-TARIFA
                                       TO WRK-CSERVC-TARIF-FILTRO
              MOVE GFCTPA-PONT-TARIFA  TO WRK-CSERVC-TARIF-INI
                                          CSERVC-TARIF     OF GFCTB0E6
              MOVE 99999               TO WRK-CSERVC-TARIF-FIM
           ELSE
              MOVE GFCTPA-FILTRO-TARIFA
                                       TO WRK-CSERVC-TARIF-FILTRO
                                          WRK-CSERVC-TARIF-INI
                                          CSERVC-TARIF     OF GFCTB0E6
                                          WRK-CSERVC-TARIF-FIM
           END-IF.

           MOVE GFCTPA-PONT-AGPTO      TO CAGPTO-CTA       OF GFCTB0E6.
           MOVE GFCTPA-PONT-CH-ALP     TO CSGL-UF          OF GFCTB0E6.
           MOVE 'N'                    TO WRK-FINALIZAR.

           IF  GFCTPA-PONT-CH-DATA     EQUAL SPACES OR LOW-VALUES
               MOVE '01.01.0001'       TO DINIC-SUSP-COBR  OF GFCTB0E6
           ELSE
               MOVE GFCTPA-PONT-CH-DATA TO DINIC-SUSP-COBR OF GFCTB0E6
           END-IF.

            EXEC SQL
                OPEN CSR05-GFCTB0E6
            END-EXEC.

            IF (SQLCODE                 NOT EQUAL ZEROS) OR
               (SQLWARN0                EQUAL 'W'      )
                MOVE SPACES             TO GFCT0M-ERRO-SQL
                INITIALIZE GFCT0M-ERRO-SQL
                MOVE 'SUSP_GRP_UF     ' TO GFCT0M-NOME-TAB
                MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
                MOVE '0150'             TO GFCT0M-LOCAL
                MOVE  0005              TO GFCTPB-COD-MSG-ERRO
                MOVE '0005'             TO GFCTG2-COD-MSG
                MOVE 'GFCTB0E6'         TO WRK-NOM-TAB
                PERFORM 21100-FORMATAR-ERRO-DB2
            END-IF.

      *----------------------------------------------------------------*
       24510-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR 5                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       24520-LER-CSR05                 SECTION.
      *----------------------------------------------------------------*

            EXEC SQL
              FETCH   CSR05-GFCTB0E6   INTO
                    :GFCTB0E6.CSERVC-TARIF,
                    :GFCTB0E6.CAGPTO-CTA,
                    :GFCTB0E6.DINIC-SUSP-COBR,
                    :GFCTB0E6.CSEQ-AGPTO-CTA,
                    :GFCTB0E6.CSGL-UF
            END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_UF '     TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTPB-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E6'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTPB-FIM
               PERFORM 24121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       24520-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR 5                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       24530-PROCESSAR-CSR05           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E6
                                       TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE CAGPTO-CTA             OF GFCTB0E6
                                       TO CAGPTO-CTA       OF GFCTB0E7.
           MOVE DINIC-SUSP-COBR        OF GFCTB0E6
                                       TO DINIC-SUSP-COBR  OF GFCTB0E7.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E6
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0E7.

           PERFORM 24131-ACESSAR-GFCTB0E7.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 24520-LER-CSR05
                   GO TO 24530-99-FIM
               END-IF
           END-IF.

           IF  IND-1                   GREATER 5
               MOVE CSERVC-TARIF       OF GFCTB0E6
                                       TO GFCTPB-PONT-TARIFA
               MOVE CAGPTO-CTA         OF GFCTB0E6
                                       TO GFCTPB-PONT-AGPTO
               MOVE CSGL-UF            OF GFCTB0E6
                                       TO GFCTPB-PONT-CH-ALP
               MOVE DINIC-SUSP-COBR    OF GFCTB0E6
                                       TO GFCTPB-PONT-CH-DATA
               MOVE 'S'                TO WRK-FINALIZAR
               GO TO 24530-99-FIM
           END-IF.

           MOVE DINIC-SUSP-COBR        OF GFCTB0E6
                                       TO GFCTPB-DATA-INICIO(IND-1).
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E6
                                       TO GFCTPB-SEQUENCIA(IND-1).
           MOVE CSGL-UF                OF GFCTB0E6
                                       TO GFCTPB-UF(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB0E6
                                       TO GFCTPB-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8 TO W-DE20
           MOVE W-DE20                 TO GFCTPB-DESC-TARIFA(IND-1).

           MOVE DFIM-SUSP-COBR OF GFCTB0E7(1:2)
                                       TO WRK-DFIM-SUSP-COBR-DIA.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7(4:2)
                                       TO WRK-DFIM-SUSP-COBR-MES.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7(7:4)
                                       TO WRK-DFIM-SUSP-COBR-ANO.

           IF WRK-DFIM-SUSP-COBR       NOT LESS WRK-DPROCM-ATUAL
               MOVE 'S'                TO GFCTPB-ATIVO(IND-1)
           ELSE
               MOVE 'N'                TO GFCTPB-ATIVO(IND-1)
           END-IF.

           PERFORM 24531-OBTER-DESC-UF.

           MOVE WRK-AREA-DCLGEN-DCITV002   TO DCITV002.
           MOVE IUF                    OF DCITV002
                                       TO GFCTPB-DESC-NOME(IND-1).

           ADD 140                     TO GFCTPB-LL.
           ADD 1                       TO GFCTPB-QTDE-TOT-REG
                                          IND-1.

           PERFORM 24520-LER-CSR05.

      *----------------------------------------------------------------*
       24530-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO DCIT8000                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       24531-OBTER-DESC-UF             SECTION.
      *----------------------------------------------------------------*

           MOVE CSGL-UF                OF GFCTB0E6
                                       TO CSGL-UF          OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTPB-FIM
               MOVE 1                  TO GFCTPB-ERRO
               MOVE ZEROS              TO GFCTPB-COD-SQL-ERRO
               MOVE  0735              TO GFCTPB-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 12100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 'S'                TO GFCTPB-FIM
               MOVE 1                  TO GFCTPB-ERRO
               MOVE ZEROS              TO GFCTPB-COD-SQL-ERRO
               MOVE  0736              TO GFCTPB-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 12100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 'S'                TO GFCTPB-FIM
               MOVE 1                  TO GFCTPB-ERRO
               MOVE ZEROS              TO GFCTPB-COD-SQL-ERRO
               MOVE  0737              TO GFCTPB-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 12100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 'S'                TO GFCTPB-FIM
               MOVE 1                  TO GFCTPB-ERRO
               MOVE ZEROS              TO GFCTPB-COD-SQL-ERRO
               MOVE  0738              TO GFCTPB-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 12100-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 4
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0047'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTPB-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 9                  TO GFCTPB-ERRO
               MOVE 'GFCT0292'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA          TO GFCT0M-SQLCA-AREA
                                          SQLCA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTPB-COD-SQL-ERRO
               MOVE SPACES             TO GFCTPB-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24531-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 5                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       24540-FECHAR-CSR05              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR05-GFCTB0E6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_GRP_UF'      TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTPB-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E6'         TO WRK-NOM-TAB
               PERFORM 21100-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       24540-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.        *
      ******************************************************************
      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
