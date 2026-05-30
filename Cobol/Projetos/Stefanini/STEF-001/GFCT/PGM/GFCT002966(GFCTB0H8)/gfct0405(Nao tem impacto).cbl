      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      *
       PROGRAM-ID. GFCT0405.
       AUTHOR.     MARCELO FARIA.
      *================================================================*
      *                     C P M - S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT0405                                     *
      *    PROGRAMADOR.:  MARCELO FARIA            - CPM PATO BRANCO   *
      *    ANALISTA CPM:  FELIPE SOMMER            - CPM PATO BRANCO   *
      *    ANALISTA....:  VALERIA                  - PROCKWORK - GP.50 *
      *    DATA........:  23/02/2006                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  FLEXIBILIZACAO - LISTA  DE  CANCELAMENTO  DE *
      *       FLEXIBILIZACAO PARA AGENCIA/CONTA POR TARIFA             *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                 TABLE                      INCLUDE/BOOK        *
      *                   DB2PRD.PDIDO_FLEXZ_CTA     GFCTB0B5          *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTJB - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTJC - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5523 *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5522 *
      *    I#GFCTF9 - AREA DE COMUNICACAO - ENTRDA E SAIDA DO GFCT5515 *
      *    I#GFCT74 - AREA DE COMUNICACAO - ENTRDA E SAIDA DO GFCT5578 *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CBLTDLI  - OBTER E ENVIAR MENSAGENS PARA IMS                *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    GFCT5522 - VERIFICA DISPONIBILIDADE ON LINE                 *
      *    GFCT5508 - VERIFICA SE AG/CONTA SAO VALIDOS                 *
      *    GFCT5515 - OBTER IDENTIFICACAO DO SOLICITANTE DE FLEXIBILI- *
      *               ZACAO, POR AGENCIA/CONTA E USUARIO.              *
      *    GFCT5578 - CLONE DO GFCT5515                                *
      *    GFCT5516 - OBTER IDENTIFICACAO DO SOLICITANTE DE FLEXIBILI- *
      *               ZACAO, POR AGENCIA/CONTA E TARIFA.               *
      *    GFCT5580 - CLONE DO GFCT5516                                *
      *    POOL0081 - ALOCACAO DINAMICA DE UM MODULO                   *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO EM 31/10/2007 - VALERIA                    *
      *  - SUBSTITUICAO DO MODULO GFCT5515 PELO GFCT5578 PARA DEPTO    *
      *    AUTORIZADO.                                                 *
      *  - SUBSTITUICAO DO MODULO GFCT5516 PELO GFCT5580 PARA DEPTO    *
      *    AUTORIZADO.                                                 *
      *================================================================*
      *    ULTIMA ALTERACAO EM 04/02/2010 - VANI                       *
      *  - INCLUSAO DAS COLUNAS LOTE E DATA SOLICITACAO NA LISTA       *
      *                                                                *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO EM 16/06/2021 -                        *
      *  - INCLUSAO DAS COLUNAS BANCO NAS QUERYS DA BASE DE PEDIDO ,
      *  COM CODIGO 237  ===> IN5731680 AFIM DE SOLUCIONAR PERFORMANCE.
      *                                                                *
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
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADOR ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC 9(003) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-SITUACAO                PIC  9(001)         VALUE ZEROS.
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE SPACES.
       77  WRK-DESPREZA                PIC  9(003)         VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.

       77  WRK-TARIFA-INI              PIC S9(005) COMP-3  VALUE ZEROS.
       77  WRK-TARIFA-FIM              PIC S9(005) COMP-3  VALUE ZEROS.

       77  WRK-DPROCM-ATUAL            PIC  X(010)         VALUE SPACES.

       01  WRK-COUNT                   PIC S9(004) COMP    VALUE ZEROS.

VAN    01  WRK-NLOTE-FLEXZ-CTA-NULL    PIC S9(004) COMP    VALUE ZEROS.

VAN    01  WRK-HSOLTC-FLEXZ            PIC  X(026)         VALUE SPACES.
VAN    01  FILLER                      REDEFINES WRK-HSOLTC-FLEXZ.
VAN        05  WRK-ANO-HSOLTC-FLEXZ    PIC  9(004).
VAN        05  FILLER                  PIC  X(001).
VAN        05  WRK-MES-HSOLTC-FLEXZ    PIC  9(002).
VAN        05  FILLER                  PIC  X(001).
VAN        05  WRK-DIA-HSOLTC-FLEXZ    PIC  9(002).
VAN        05  FILLER                  PIC  X(016).

VAN    01  WRK-DATA-DB2.
VAN        05  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
VAN        05  FILLER                  PIC  X(001)         VALUE '/'.
VAN        05  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
VAN        05  FILLER                  PIC  X(001)         VALUE '/'.
VAN        05  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           03  FILLER                  PIC  X(025)         VALUE
               'ERRO DE ACESSO AO MODULO '.
           03  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO GFCT5508 ***'.
      *----------------------------------------------------------------*

       01  WRK-5508-AREA-ENTRADA.
           05  WRK-5508-AMBIENTE       PIC  X(001)         VALUE SPACES.
           05  WRK-5508-CDEPDC-CTA-CLI PIC  9(005)         VALUE ZEROS.
           05  WRK-5508-CNRO-CTA-CLI   PIC  9(007)         VALUE ZEROS.

       01  WRK-5508-AREA-SAIDA.
           05  WRK-5508-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5508-COD-SQL-ERRO   PIC  9(003)         VALUE ZEROS.
           05  WRK-5508-DESC-MSG       PIC  X(070)         VALUE SPACES.
           05  WRK-5508-DADOS-RETORNO.
            10  WRK-5508-IPSSOA-COPLT  PIC  X(070)         VALUE SPACES.

       01  WRK-IO-PCB                  PIC  X(032)         VALUE SPACES.

       01  WRK-ALT-PCB                 PIC  X(032)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
0907***     '*    AREA DO MODULO GFCT5515   *'.
0907        '*    AREA DO MODULO GFCT5578   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT74'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
0907***    '*** AREA DO MODULO GFCT5516 ***'.
0907       '*** AREA DO MODULO GFCT5580 ***'.
      *----------------------------------------------------------------*

       01  WRK-5580-AREA-ENTRADA.
           05  WRK-5580-AMBIENTE           PIC  X(001)     VALUE SPACES.
           05  WRK-5580-COD-FUNC           PIC  X(007)     VALUE SPACES.
           05  WRK-5580-AGENCIA            PIC  9(005)     VALUE ZEROS.
           05  WRK-5580-CONTA              PIC  9(007)     VALUE ZEROS.
           05  WRK-5580-TARIFA             PIC  9(005)     VALUE ZEROS.

       01  WRK-5580-AREA-SAIDA.
           05  WRK-5580-COD-RETORNO        PIC  9(002)     VALUE ZEROS.
           05  WRK-5580-COD-SQL-ERRO       PIC  9(003)     VALUE ZEROS.
           05  WRK-5580-DESC-MSG           PIC  X(070)     VALUE SPACES.
           05  WRK-5580-DADOS-RETORNO.
            10  WRK-5580-TIPO-DEPENDENCIA  PIC  X(002)     VALUE SPACES.
            10  WRK-5580-CODIGO-JUNCAO     PIC  9(005)     VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO GFCT5522 ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO GFCT5523 ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TABELAS DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B5
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CURSORES ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE CSR-GFCTB0B5 CURSOR FOR
               SELECT
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_CTA,
                   DINIC_FLEXZ_CTA,
                   DFIM_FLEXZ_CTA,
                   QFRANQ_FLEXZ_CTA,
                   CPER_FRANQ_CTA,
                   VNEGOC_FLEXZ_CTA,
                   PDESC_FLEXZ_CTA,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_CTA,
                   CDEPDC_ORIGE_FLEXZ,
                   CDEPDC_PAREC_FLEXZ,
VAN                HSOLTC_FLEXZ,
VAN                CSIT_SOLTC_FLEXZ,
VAN                NLOTE_FLEXZ_CTA
               FROM DB2PRD.PDIDO_FLEXZ_CTA
               WHERE
INPERF              CBCO_CTA_CLI           = :GFCTB0B5.CBCO-CTA-CLI
               AND  CJUNC_DEPDC            = :GFCTB0B5.CJUNC-DEPDC
               AND  CCTA_CLI               = :GFCTB0B5.CCTA-CLI
               AND (CSERVC_TARIF          >= :WRK-TARIFA-INI
               AND  CSERVC_TARIF          <= :WRK-TARIFA-FIM)
               AND ((CSIT_SOLTC_FLEXZ      = 01
               AND   CDEPDC_ORIGE_FLEXZ    =
                                          :GFCTB0B5.CDEPDC-ORIGE-FLEXZ)
                OR  (CSIT_SOLTC_FLEXZ      = 02
               AND   CDEPDC_PAREC_FLEXZ    =
                                          :GFCTB0B5.CDEPDC-PAREC-FLEXZ
022011          OR  (CSIT_SOLTC_FLEXZ      = 10
022011         AND   CDEPDC_PAREC_FLEXZ    =
022011                                    :GFCTB0B5.CDEPDC-PAREC-FLEXZ
               AND   DINIC_FLEXZ_CTA       > :WRK-DPROCM-ATUAL)
                OR  (CSIT_SOLTC_FLEXZ      = 07
               AND   ((CDEPDC_PAREC_FLEXZ <> 0
               AND     CDEPDC_PAREC_FLEXZ  =
                                        :GFCTB0B5.CDEPDC-PAREC-FLEXZ)
                OR    (CDEPDC_PAREC_FLEXZ  = 0
               AND     CDEPDC_ORIGE_FLEXZ  =
                                      :GFCTB0B5.CDEPDC-ORIGE-FLEXZ)))))
               ORDER BY CSERVC_TARIF,
                        DINIC_FLEXZ_CTA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTJB'.

       COPY 'I#GFCTJC'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTJB-ENTRADA
                                             GFCTJC-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL DO PROGRAMA.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO DE VARIAVEIS.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJC-SAIDA
                                          GFCTG2-ENTRADA
                                          GFCTG3-SAIDA
                                          GFCTFZ-ENTRADA
                                          GFCTG1-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTJC-SAIDA
                      GFCTG2-ENTRADA
                      GFCTG3-SAIDA
                      GFCTFZ-ENTRADA
                      GFCTG1-SAIDA
                      GFCT0M-AREA-ERROS
                      IND-1.

           PERFORM 1100-INICIALIZAR-AREA-SAIDA.

           PERFORM 1200-VERIFICAR-SIST-DISPONIVEL.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-OBTER-DATA-DPROCM.

           PERFORM 1500-VALIDAR-AGENCIA-CONTA.

           IF  GFCTJB-TARIFA-FIL       EQUAL ZEROS
0907***        PERFORM 1600-ACESSAR-GFCT5515
0907           PERFORM 1600-ACESSAR-GFCT5578

               MOVE GFCT74-DPTO        TO CDEPDC-ORIGE-FLEXZ
                                                            OF GFCTB0B5
                                          CDEPDC-PAREC-FLEXZ
                                                            OF GFCTB0B5
           ELSE
0907***        PERFORM 1700-ACESSAR-GFCT5516
0907           PERFORM 1700-ACESSAR-GFCT5580

               MOVE WRK-5580-CODIGO-JUNCAO
                                       TO CDEPDC-ORIGE-FLEXZ
                                                            OF GFCTB0B5
                                          CDEPDC-PAREC-FLEXZ
                                                            OF GFCTB0B5
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZAR A AREA GFCTJC-SAIDA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-INICIALIZAR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

VAN        MOVE +296                   TO GFCTJC-LL.
           MOVE GFCTJB-ZZ              TO GFCTJC-ZZ.
           MOVE GFCTJB-TRANSACAO       TO GFCTJC-TRANSACAO.
           MOVE GFCTJB-FUNCAO          TO GFCTJC-FUNCAO.

           IF  GFCTJB-QTDE-OCOR        NOT NUMERIC OR
               GFCTJB-QTDE-OCOR        EQUAL ZEROS
               MOVE ZEROS              TO GFCTJC-QTDE-OCOR
           ELSE
               MOVE GFCTJB-QTDE-OCOR   TO GFCTJC-QTDE-OCOR
           END-IF.

           IF  GFCTJB-QTDE-TOT-REG     NOT NUMERIC OR
               GFCTJB-QTDE-TOT-REG     EQUAL ZEROS
               MOVE ZEROS              TO GFCTJC-QTDE-TOT-REG
           ELSE
               MOVE GFCTJB-QTDE-TOT-REG TO GFCTJC-QTDE-TOT-REG
           END-IF.

           MOVE  GFCTJB-FUNC-BDSCO     TO GFCTJC-FUNC-BDSCO.
           MOVE  GFCTJB-FILTRO         TO GFCTJC-FILTRO.
           MOVE  GFCTJB-PONTEIRO       TO GFCTJC-PONTEIRO.
           MOVE 'N'                    TO GFCTJC-FIM.

           MOVE ZEROS                  TO GFCTJC-ERRO
                                          GFCTJC-COD-SQL-ERRO
                                          GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTJC-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR SITUACAO DO SISTEMA.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-VERIFICAR-SIST-DISPONIVEL  SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5522'                 TO WRK-MODULO.
           MOVE 'GFCT'                     TO GFCTFZ-SISTEMA.
           MOVE  GFCTJB-TRANSACAO          TO GFCTFZ-TRANSACAO.
           MOVE  GFCTJB-FUNCAO             TO GFCTFZ-FUNCAO.
           MOVE  ZEROS                     TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                        TO GFCTFZ-FIM.

           CALL 'POOL0081'                 USING WRK-MODULO
                                                 GFCTFZ-ENTRADA
                                                 GFCTG1-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-NOME-MOD
               MOVE WRK-MSG01              TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0405'             TO GFCT0M-TRANSACAO
               MOVE 9                      TO GFCTJC-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 'GFCT0405'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              GFCTJC-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTJC-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTJC-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCTJC-DESC-MSG-ERRO
                                              GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0405'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTJC-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTIR DADOS DE ENTRADA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTJB-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTJB-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTJB-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTJB-AGENCIA-FIL      NOT NUMERIC)                OR
              (GFCTJB-AGENCIA-FIL      EQUAL ZEROS)                OR
              (GFCTJB-CONTA-FIL        NOT NUMERIC)                OR
              (GFCTJB-CONTA-FIL        EQUAL ZEROS)                OR
              (GFCTJB-TARIFA-FIL       NOT NUMERIC)                OR
              (GFCTJB-AGENCIA-PONT     NOT NUMERIC)                OR
              (GFCTJB-CONTA-PONT       NOT NUMERIC)                OR
              (GFCTJB-TARIFA-PONT      NOT NUMERIC)
               MOVE 'S'                TO GFCTJC-FIM
               MOVE 1                  TO GFCTJC-ERRO
               MOVE ZEROS              TO GFCTJC-COD-SQL-ERRO
               MOVE SPACES             TO GFCTJC-DESC-MSG-ERRO
               MOVE 0001               TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG      TO GFCTJC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER DESCRICAO DA MENSAGEM.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTJB-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTJB-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTJB-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0405'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTJC-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 9
               MOVE GFCTG3-ERRO                TO GFCTJC-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MOD
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0405'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 1
               MOVE 'S'                        TO GFCTJC-FIM
               MOVE GFCTG3-ERRO                TO GFCTJC-ERRO
               MOVE GFCTG3-COD-SQL-ERRO        TO GFCTJC-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO        TO GFCTJC-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO       TO GFCTJC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER DATA DO PROCESSAMENTO.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-OBTER-DATA-DPROCM          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
               SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
               INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
               FROM DB2PRD.PARM_DATA_PROCM
               WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0A1'      TO WRK-NOME-TAB
               MOVE 'GFCT0405'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTJC-ERRO
               MOVE  SQLCODE           TO GFCTJC-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTJC-DESC-MSG-ERRO
               MOVE  0010              TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONCATENAR MENSAGEM.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1410-CONCATENAR-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO GFCTJC-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VALIDAR AGENCIA E CONTA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-VALIDAR-AGENCIA-CONTA      SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5508-AMBIENTE.
           MOVE  GFCTJB-AGENCIA-FIL    TO WRK-5508-CDEPDC-CTA-CLI.
           MOVE  GFCTJB-CONTA-FIL      TO WRK-5508-CNRO-CTA-CLI.
           MOVE 'GFCT5508'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5508-AREA-ENTRADA
                                             WRK-5508-AREA-SAIDA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0405'         TO GFCT0M-PROGRAMA
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTJC-DESC-MSG-ERRO
               MOVE 9                  TO GFCTJC-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5508-COD-RETORNO    EQUAL 02
               MOVE  'S'               TO GFCTJC-FIM
               MOVE  1                 TO GFCTJC-ERRO
               MOVE  ZEROS             TO GFCTJC-COD-SQL-ERRO
               MOVE  0221              TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTJC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
0907*** ROTINA PARA ACESSAR MODULO GFCT5515 - GESTOR LOGADO.           *
0907  * ROTINA PARA ACESSAR MODULO GFCT5578 - GESTOR LOGADO.           *
      ******************************************************************
      *----------------------------------------------------------------*
0907***1600-ACESSAR-GFCT5515           SECTION.
0907   1600-ACESSAR-GFCT5578           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCT74-GFCT5578.

           MOVE GFCTJB-AGENCIA-FIL     TO GFCT74-AGENCIA.
           MOVE GFCTJB-CONTA-FIL       TO GFCT74-CONTA.
           MOVE GFCTJB-FUNC-BDSCO      TO GFCT74-COD-FUNC.

0907***    MOVE 'GFCT5515'             TO WRK-MODULO.
0907       MOVE 'GFCT5578'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT74-GFCT5578.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0405'         TO GFCT0M-PROGRAMA
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTJC-DESC-MSG-ERRO
               MOVE 9                  TO GFCTJC-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT74-COD-RETORNO      NOT EQUAL ZEROS AND 10
               MOVE 'S'                TO GFCTJC-FIM
               MOVE 1                  TO GFCTJC-ERRO

               PERFORM 1610-VERIFICAR-CODIGO

               PERFORM 1310-OBTER-DESC-MSG

               MOVE  GFCTG3-DESC-MSG   TO GFCTJC-DESC-MSG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
0907*** ROTINA PARA VERIFICAR CODIGO DE RETORNO DO MODULO GFCT5515.    *
0907*** ROTINA PARA VERIFICAR CODIGO DE RETORNO DO MODULO GFCT5578.    *
      ******************************************************************
      *----------------------------------------------------------------*
       1610-VERIFICAR-CODIGO           SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT74-COD-RETORNO
               WHEN 02
                 MOVE 0435             TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 03
                 MOVE 0436             TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 04
                 MOVE 0437             TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 06
                 MOVE 0438             TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 07
                 MOVE 0439             TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 08
                 MOVE 0440             TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 09
                 MOVE 0441             TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 11
                 MOVE 0810             TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
           END-EVALUATE.

      *----------------------------------------------------------------*
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
0907*** ROTINA PARA ACESSAR MODULO GFCT5516 - GESTOR LOGADO.           *
0907*** ROTINA PARA ACESSAR MODULO GFCT5580 - GESTOR LOGADO.           *
      ******************************************************************
      *----------------------------------------------------------------*
0907***1700-ACESSAR-GFCT5516           SECTION.
0907   1700-ACESSAR-GFCT5580           SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                        TO WRK-5580-AMBIENTE.
           MOVE  GFCTJB-FUNC-BDSCO         TO WRK-5580-COD-FUNC.
           MOVE  GFCTJB-AGENCIA-FIL        TO WRK-5580-AGENCIA.
           MOVE  GFCTJB-CONTA-FIL          TO WRK-5580-CONTA.
           MOVE  GFCTJB-TARIFA-FIL         TO WRK-5580-TARIFA.
0907***    MOVE 'GFCT5516'                 TO WRK-MODULO.
0907       MOVE 'GFCT5580'                 TO WRK-MODULO.

           CALL 'POOL0081'                 USING WRK-MODULO
                                                 WRK-5580-AREA-ENTRADA
                                                 WRK-5580-AREA-SAIDA
                                                 GFCT0M-AREA-ERROS
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0405'             TO GFCT0M-PROGRAMA
               MOVE WRK-MODULO             TO WRK-NOME-MOD
               MOVE WRK-MSG01              TO GFCT0M-TEXTO
                                              GFCTJC-DESC-MSG-ERRO
               MOVE 9                      TO GFCTJC-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5580-COD-RETORNO        NOT EQUAL 00 AND 03 AND 10
               MOVE  9                     TO GFCTJC-ERRO
               MOVE  WRK-5580-COD-SQL-ERRO TO GFCTJC-COD-SQL-ERRO
               MOVE  WRK-5580-DESC-MSG     TO GFCTJC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5580-COD-RETORNO        EQUAL 03
               MOVE  'S'                   TO GFCTJC-FIM
               MOVE  1                     TO GFCTJC-ERRO
               MOVE  0436                  TO GFCTJC-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTJC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5580-COD-RETORNO        EQUAL 10
               MOVE  'S'                   TO GFCTJC-FIM
               MOVE  1                     TO GFCTJC-ERRO
               MOVE  0442                  TO GFCTJC-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTJC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAMENTO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTJB-AGENCIA-FIL        TO CJUNC-DEPDC   OF GFCTB0B5.

           MOVE  GFCTJB-CONTA-FIL          TO CCTA-CLI      OF GFCTB0B5.

           IF  GFCTJB-TARIFA-FIL           EQUAL ZEROS
               MOVE ZEROS                  TO WRK-TARIFA-INI
               MOVE 99999                  TO WRK-TARIFA-FIM
           ELSE
               MOVE GFCTJB-TARIFA-FIL      TO WRK-TARIFA-INI
                                              WRK-TARIFA-FIM
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1   TO WRK-DPROCM-ATUAL.

INPER      MOVE 237                        TO CBCO-CTA-CLI
                                           OF GFCTB0B5.

           PERFORM 2100-PROCESSAR-SELECT-COUNT.

           IF  WRK-COUNT               GREATER 150
               MOVE  ZEROS             TO GFCTJC-ERRO
                                          GFCTJC-COD-SQL-ERRO
               MOVE 0662               TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTJC-DESC-MSG-ERRO
               MOVE 'S'                TO WRK-FIM-CURSOR
               GO TO 2300-99-FIM
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CURSOR

           PERFORM 2200-ABRIR-CURSOR.

           COMPUTE WRK-DESPREZA        = GFCTJB-QTDE-OCOR + 1.

           PERFORM 2300-LER-CURSOR
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER WRK-DESPREZA
                OR WRK-FIM-CURSOR      EQUAL 'S'.

           IF  WRK-FIM-CURSOR          EQUAL 'S'
               MOVE 'S'                TO GFCTJC-FIM
               MOVE  ZEROS             TO GFCTJC-ERRO
                                          GFCTJC-COD-SQL-ERRO
               MOVE  0007              TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTJC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2400-PROCESSAR-CURSOR
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER 7
                OR WRK-FIM-CURSOR      EQUAL 'S'.

           PERFORM 2500-FECHAR-CURSOR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR SELECT COUNT NA TABELA GFCTB0B5.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSAR-SELECT-COUNT     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT COUNT(*)
               INTO   :WRK-COUNT
               FROM   DB2PRD.PDIDO_FLEXZ_CTA
               WHERE
INPERF              CBCO_CTA_CLI           = :GFCTB0B5.CBCO-CTA-CLI
               AND  CJUNC_DEPDC            = :GFCTB0B5.CJUNC-DEPDC
               AND  CCTA_CLI               = :GFCTB0B5.CCTA-CLI
               AND (CSERVC_TARIF          >= :WRK-TARIFA-INI
               AND  CSERVC_TARIF          <= :WRK-TARIFA-FIM)
               AND ((CSIT_SOLTC_FLEXZ      = 01
               AND   CDEPDC_ORIGE_FLEXZ    =
                                          :GFCTB0B5.CDEPDC-ORIGE-FLEXZ)
                OR  (CSIT_SOLTC_FLEXZ      = 02
               AND   CDEPDC_PAREC_FLEXZ    =
                                          :GFCTB0B5.CDEPDC-PAREC-FLEXZ
022011          OR  (CSIT_SOLTC_FLEXZ      = 10
022011         AND   CDEPDC_PAREC_FLEXZ    =
022011                                    :GFCTB0B5.CDEPDC-PAREC-FLEXZ
               AND   DINIC_FLEXZ_CTA       > :WRK-DPROCM-ATUAL)
                OR  (CSIT_SOLTC_FLEXZ      = 07
               AND   ((CDEPDC_PAREC_FLEXZ <> 0
               AND     CDEPDC_PAREC_FLEXZ  =
                                        :GFCTB0B5.CDEPDC-PAREC-FLEXZ)
                OR    (CDEPDC_PAREC_FLEXZ  = 0
               AND     CDEPDC_ORIGE_FLEXZ  =
                                      :GFCTB0B5.CDEPDC-ORIGE-FLEXZ)))))
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0B5'      TO WRK-NOME-TAB
               MOVE 'GFCT0457'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TDEB_UNFCA_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTJC-ERRO
               MOVE  SQLCODE           TO GFCTJC-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTJC-DESC-MSG-ERRO
               MOVE  0010              TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ABERTURA DE CURSORES.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR-GFCTB0B5
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0B5'       TO WRK-NOME-TAB
               MOVE 'GFCT0405'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CTA'   TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0030'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTJC-ERRO
               MOVE  SQLCODE            TO GFCTJC-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTJC-DESC-MSG-ERRO
               MOVE  0010               TO GFCTJC-COD-MSG-ERRO
                                           GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LEITURA DE CURSORES.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH CSR-GFCTB0B5 INTO
                     :GFCTB0B5.CJUNC-DEPDC,
                     :GFCTB0B5.CCTA-CLI,
                     :GFCTB0B5.CSERVC-TARIF,
                     :GFCTB0B5.HIDTFD-FLEXZ-CTA,
                     :GFCTB0B5.DINIC-FLEXZ-CTA,
                     :GFCTB0B5.DFIM-FLEXZ-CTA,
                     :GFCTB0B5.QFRANQ-FLEXZ-CTA,
                     :GFCTB0B5.CPER-FRANQ-CTA,
                     :GFCTB0B5.VNEGOC-FLEXZ-CTA,
                     :GFCTB0B5.PDESC-FLEXZ-CTA,
                     :GFCTB0B5.DDIA-AGNDA,
                     :GFCTB0B5.QDIA-FLEXZ-CTA,
                     :GFCTB0B5.CDEPDC-ORIGE-FLEXZ,
                     :GFCTB0B5.CDEPDC-PAREC-FLEXZ,
VAN                  :GFCTB0B5.HSOLTC-FLEXZ,
VAN                  :GFCTB0B5.CSIT-SOLTC-FLEXZ,
VAN                  :GFCTB0B5.NLOTE-FLEXZ-CTA
VAN                           :WRK-NLOTE-FLEXZ-CTA-NULL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -305) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0B5'      TO WRK-NOME-TAB
               MOVE 'GFCT0405'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTJC-ERRO
               MOVE  SQLCODE           TO GFCTJC-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTJC-DESC-MSG-ERRO
               MOVE  0006              TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 1410-CONCATENAR-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTJC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO GFCTJC-FIM
                                          WRK-FIM-CURSOR
               IF  IND-1               EQUAL ZEROS
                   MOVE  ZEROS         TO GFCTJC-ERRO
                                          GFCTJC-COD-SQL-ERRO
                   MOVE  0007          TO GFCTJC-COD-MSG-ERRO
                                          GFCTG2-COD-MSG

                   PERFORM 1310-OBTER-DESC-MSG

                   MOVE GFCTG3-DESC-MSG TO GFCTJC-DESC-MSG-ERRO
               END-IF
VAN        ELSE
VAN            IF  WRK-NLOTE-FLEXZ-CTA-NULL
VAN                                    LESS ZEROS
VAN                MOVE ZEROS          TO NLOTE-FLEXZ-CTA OF GFCTB0B5
VAN            END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESAR CURSORES.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-MOVER-CAMPOS.

           PERFORM 2300-LER-CURSOR.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA MOVIMENTACAO DE CAMPOS PARA INC DE SAIDA.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-MOVER-CAMPOS               SECTION.
      *----------------------------------------------------------------*

VAN        ADD 102                     TO GFCTJC-LL.

           MOVE  CSERVC-TARIF          OF GFCTB0B5
                                       TO GFCTJC-COD-TARIFA(IND-1).
           MOVE  HIDTFD-FLEXZ-CTA      OF GFCTB0B5
                                       TO GFCTJC-TIMESTAMP(IND-1).
           MOVE  DINIC-FLEXZ-CTA       OF GFCTB0B5
                                       TO GFCTJC-INIC-VIG-FLEX(IND-1).
           MOVE  DFIM-FLEXZ-CTA        OF GFCTB0B5
                                       TO GFCTJC-FIM-VIG-FLEX(IND-1).
           MOVE  QFRANQ-FLEXZ-CTA      OF GFCTB0B5
                                       TO GFCTJC-FRANQUIA(IND-1).
           MOVE  CPER-FRANQ-CTA        OF GFCTB0B5
                                       TO GFCTJC-PERIODO(IND-1).
           MOVE  VNEGOC-FLEXZ-CTA      OF GFCTB0B5
                                       TO GFCTJC-VR-NEGOCIADO(IND-1).
           MOVE  PDESC-FLEXZ-CTA       OF GFCTB0B5
                                       TO GFCTJC-PERC-DESCTO(IND-1).
           MOVE  DDIA-AGNDA            OF GFCTB0B5
                                       TO GFCTJC-DIA-AGEND(IND-1).
           MOVE  QDIA-FLEXZ-CTA        OF GFCTB0B5
                                       TO GFCTJC-DIAS-RETENCAO(IND-1).
           MOVE  CSIT-SOLTC-FLEXZ      OF GFCTB0B5
                                       TO GFCTJC-SITUACAO-FLEX(IND-1).
           MOVE  CDEPDC-ORIGE-FLEXZ    OF GFCTB0B5
                                       TO GFCTJC-JUNCAO-SOLIC(IND-1).

VAN        MOVE  HSOLTC-FLEXZ          OF GFCTB0B5
VAN                                    TO WRK-HSOLTC-FLEXZ.
VAN        MOVE  WRK-DIA-HSOLTC-FLEXZ  TO WRK-DIA-DB2.
VAN        MOVE  WRK-MES-HSOLTC-FLEXZ  TO WRK-MES-DB2.
VAN        MOVE  WRK-ANO-HSOLTC-FLEXZ  TO WRK-ANO-DB2.
VAN        MOVE  WRK-DATA-DB2          TO GFCTJC-DT-SOLICT(IND-1).

VAN        MOVE  NLOTE-FLEXZ-CTA       OF GFCTB0B5
VAN                                    TO GFCTJC-NRO-LOTE(IND-1).

           ADD 1                       TO GFCTJC-QTDE-OCOR
                                          GFCTJC-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FECHAMENTO DE CURSORES.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-GFCTB0B5
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0B5'       TO WRK-NOME-TAB
               MOVE 'GFCT0405'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CTA'   TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0050'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTJC-ERRO
               MOVE  SQLCODE            TO GFCTJC-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTJC-DESC-MSG-ERRO
               MOVE  0011               TO GFCTJC-COD-MSG-ERRO
                                           GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
