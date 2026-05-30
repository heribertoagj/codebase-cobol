      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0147.
       AUTHOR.     SONDAPROCWORK OUTSOURCING.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0147                                    *
      *    ANALISTA....:   WAGNER SILVA            - PROCWORK - GP.50  *
      *    DATA........:   26/03/2009                                  *
      *                                                                *
      *    OBJETIVO    :   CONSULTA DETALHADA DAS SOLICITACOES DE      *
      *                    EXCLUSAO MASSIVA DE ADESAO.                 *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *           DB2PRD.TCANCT_ADSAO_AGPTO          GFCTB0L0          *
      *           DB2PRD.TCANCT_ADSAO_EMPR           GFCTB0L1          *
      *           DB2PRD.TCANCT_ADSAO_FORNC          GFCTB0L2          *
      *           DB2PRD.TCANCT_ADSAO_POSTO          GFCTB0L3          *
      *           DB2PRD.SERVC_TARIF_PRINC           GFCTB0D8          *
      *           DB2PRD.TPO_AGPTO_CTA               GFCTB0F3          *
      *           DB2PRD.CLIENTE_PAGADOR             PFEBB000          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5522 - VERIFICACAO DE DISPONIBILIDADE ON-LINE         *
      *      GFCT5006 - OBTER DESCRICAO DA AGENCIA                     *
      *      GFCT5060 - OBTER DESCRICAO DA EMPRESA                     *
      *      GFCT5090 - OBTER DESCRICAO DO CORRESPONDENTE BANCARIO     *
      *      GFCT5109 - OBTER DESCRICAO DO PAB                         *
      *      SARH8000 - MODULO PARA PERMITIR AOS SISTEMAS LEGADOS O    *
      *                 ACESSO AS INFORMACOES DE FUNCIONARIOS.         *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7148                                     *
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
       01  FILLER                      PIC  X(032)         VALUE
           '*  INICIO DA WORKING GFCT0147  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-COD-TABELA              PIC  X(008)         VALUE SPACES.

       01  WRK-SQLCODE                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE.
           05  FILLER                  PIC  9(006).
           05  WRK-COD-SQL             PIC  9(003).

       01  WRK-AGENCIA                 PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AGENCIA.
           05  WRK-COD-AGENCIA         PIC  9(005).

       01  WRK-POSTO                   PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-POSTO.
           05  WRK-COD-POSTO           PIC  9(005).

       01  WRK-CONTA                   PIC S9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CONTA.
           05  WRK-COD-CONTA           PIC  9(013).
           05  FILLER                  REDEFINES WRK-COD-CONTA.
               10  FILLER              PIC  9(006).
               10  WRK-COD-CONTA-TAM-7 PIC  9(007).

ST2506*01  WRK-CCNPJ-CPF               PIC S9(009)         VALUE ZEROS.
ST2506*01  FILLER                      REDEFINES WRK-CCNPJ-CPF.
ST2506*    05  WRK-COD-CNPJ            PIC  9(009).

ST2506*01  WRK-CFLIAL-CNPJ             PIC S9(005)         VALUE ZEROS.
ST2506*01  FILLER                      REDEFINES WRK-CFLIAL-CNPJ.
ST2506*    05  WRK-COD-FILIAL          PIC  9(005).

       01  WRK-CCTRL-CNPJ              PIC S9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CCTRL-CNPJ.
           05  WRK-COD-CONTROLE        PIC  9(002).

       01  WRK-QPRVIO-CANCT-ADSAO      PIC S9(011)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-QPRVIO-CANCT-ADSAO.
           05  WRK-QTD-PREVIA          PIC  9(011).

       01  WRK-QADSAO-CANCD            PIC S9(011)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-QADSAO-CANCD.
           05  WRK-QTD-EXCLUIDAS       PIC  9(011).

       01  WRK-CFUNC                   PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CFUNC.
           05  WRK-COD-FUNC            PIC  9(009).
           05  FILLER                  REDEFINES WRK-COD-FUNC.
               10  FILLER              PIC  9(002).
               10  WRK-COD-FUNC-TAM-7  PIC  9(007).

       01  WRK-COD-FUNC-NUM            PIC  9(007)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-COD-FUNC-NUM.
           05  WRK-COD-FUNC-X          PIC  X(007).

       01  WRK-NOME-FUNC               PIC  X(030)         VALUE SPACES.

       01  WRK-TIMESTAMP.
           05  WRK-ANO-TIMESTAMP       PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-TIMESTAMP       PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-DIA-TIMESTAMP       PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-HOR-TIMESTAMP       PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MIN-TIMESTAMP       PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(010)         VALUE SPACES.

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

       01  WRK-HORA-AUX.
           03  WRK-HOR-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  WRK-MIN-AUX             PIC  9(002)         VALUE ZEROS.

       01  WRK-ICLI-NULL               PIC S9(004) COMP    VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MSG001.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO '.
           05  WRK-MSG001-MODULO       PIC  X(008)         VALUE SPACES.

       01  WRK-MSG002.
           05  FILLER                  PIC  X(026)         VALUE
               'SISTEMA INDISPONIVEL'.

       01  WRK-MSG003.
           05  FILLER                  PIC  X(030)         VALUE
               'FUNCIONARIO NAO ENCONTRADO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO SARH8000   *'.
      *----------------------------------------------------------------*

       01  WRK-ENTRADA.
           05  WRK-PESQUISA-ENT        PIC  9(009)         VALUE ZEROS.
           05  WRK-TAMANHO-ENT         PIC  9(004)         VALUE ZEROS.
           05  WRK-SOLICITA-ENT.
               10  FILLER              OCCURS 255 TIMES.
                   15  WRK-CAMPO-ENT   PIC  9(003)         VALUE ZEROS.

       01  WRK-SAIDA.
           05  WRK-COD-RETORNO-SAI     PIC  9(002)         VALUE ZEROS.
           05  WRK-TAMANHO-SAI         PIC  9(004)         VALUE ZEROS.
           05  WRK-REGISTRO-SAI.
               10  FILLER              OCCURS 0 TO 1000 TIMES
                                       DEPENDING ON WRK-TAMANHO-SAI.
                   15  WRK-POSICAO-SAI PIC  X(001)         VALUE SPACES.

       01  WRK-ERRO-AREA               PIC  X(107)         VALUE SPACES.

       01  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO GFCT5522   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.
       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.
       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* AREA DO MODULO GFCT5109/5090 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGB'.
       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO GFCT5006   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1V'.
       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO GFCT5060   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTBR'.
       COPY 'I#GFCTBS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     AREAS DE TABELAS DB2     *'.
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
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0F3
           END-EXEC.

           EXEC SQL
                INCLUDE PFEBB000
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    FIM DA WORKING GFCT0147   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAFP.
       COPY GFCTWAFQ.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING WAFP-ENTRADA
                                             WAFQ-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 9999-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-MONTAR-AREA-SAIDA.

           PERFORM 1200-VERIFICAR-ONLINE.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAFQ-SAIDA.

           INITIALIZE                  WAFQ-SAIDA.

           MOVE +1010                  TO WAFQ-LL.
           MOVE WAFP-TRANSACAO         TO WAFQ-TRANSACAO.
           MOVE WAFP-FUNCAO            TO WAFQ-FUNCAO.

           MOVE WAFP-FUNC-BDSCO        TO WAFQ-FUNC-BDSCO.
           MOVE WAFP-TERMINAL          TO WAFQ-TERMINAL.
           MOVE 'S'                    TO WAFQ-FIM.

           MOVE ZEROS                  TO WAFQ-ERRO
                                          WAFQ-COD-SQL-ERRO
                                          WAFQ-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 9990-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO WAFQ-DESC-MSG-ERRO.

           MOVE WAFP-HINCL             TO WAFQ-HINCL.
           MOVE WAFP-CAGPTO            TO WAFQ-CAGPTO.
           MOVE WAFP-RAGPTO            TO WAFQ-RAGPTO.
           MOVE WAFP-CPACOTE           TO WAFQ-CPACOTE.
           MOVE WAFP-RPACOTE           TO WAFQ-RPACOTE.
           MOVE WAFP-CAGENCIA          TO WAFQ-CAGENCIA.
           MOVE WAFP-RAGENCIA          TO WAFQ-RAGENCIA.
           MOVE WAFP-CPOSTO            TO WAFQ-CPOSTO.
           MOVE WAFP-RPOSTO            TO WAFQ-RPOSTO.
           MOVE WAFP-CCONTA            TO WAFQ-CCONTA.
           MOVE WAFP-RCONTA            TO WAFQ-RCONTA.
ST2506*    MOVE WAFP-CNPJ-NUM          TO WAFQ-CNPJ-NUM.
ST2506     MOVE WAFP-CNPJ-CGC          TO WAFQ-CNPJ-CGC.
           MOVE WAFP-CNPJ-FIL          TO WAFQ-CNPJ-FIL.
           MOVE WAFP-CNPJ-CTR          TO WAFQ-CNPJ-CTR.
           MOVE WAFP-RCNPJ             TO WAFQ-RCNPJ.
           MOVE WAFP-DATA-SOLIC        TO WAFQ-DATA-SOLIC.
           MOVE WAFP-COMENT            TO WAFQ-COMENT.
           MOVE WAFP-CFUNC-SOLIC       TO WAFQ-CFUNC-SOLIC.
           MOVE WAFP-RFUNC-SOLIC       TO WAFQ-RFUNC-SOLIC.
           MOVE WAFP-DATA-INCL         TO WAFQ-DATA-INCL.
           MOVE WAFP-HORA-INCL         TO WAFQ-HORA-INCL.
           MOVE WAFP-QTD-PREVIA        TO WAFQ-QTD-PREVIA.
           MOVE WAFP-QTD-EXCLUIDAS     TO WAFQ-QTD-EXCLUIDAS.
           MOVE WAFP-CFUNC-CANC-PAREC  TO WAFQ-CFUNC-CANC-PAREC.
           MOVE WAFP-RFUNC-CANC-PAREC  TO WAFQ-RFUNC-CANC-PAREC.
           MOVE WAFP-DATA-CANC-PAREC   TO WAFQ-DATA-CANC-PAREC.
           MOVE WAFP-HORA-CANC-PAREC   TO WAFQ-HORA-CANC-PAREC.
           MOVE WAFP-SITUACAO          TO WAFQ-SITUACAO.
           MOVE WAFP-PARECER           TO WAFQ-PARECER.
           MOVE WAFP-JUSTIF            TO WAFQ-JUSTIF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-VERIFICAR-ONLINE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA
                                          GFCTFZ-ENTRADA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTG1-SAIDA
                                       GFCTFZ-ENTRADA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE WAFP-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAFP-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAFP-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFQ-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0147'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9

               MOVE 9                  TO WAFQ-ERRO

               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG1-COD-MSG-ERRO
                                       TO WAFQ-COD-MSG-ERRO
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAFQ-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WAFQ-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0147'     TO GFCT0M-TRANSACAO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAFQ-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0147'         TO GFCT0M-TRANSACAO
               MOVE WRK-MSG002         TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (WAFP-TRANSACAO          EQUAL SPACES OR LOW-VALUES) OR
              (WAFP-FUNCAO             EQUAL SPACES OR LOW-VALUES) OR
              (WAFP-FUNC-BDSCO         EQUAL SPACES OR LOW-VALUES) OR
              (WAFP-HINCL              EQUAL SPACES OR LOW-VALUES) OR
              (WAFP-CAGPTO             NOT NUMERIC)                OR
              (WAFP-CAGPTO             EQUAL ZEROS)                OR
              (WAFP-CPACOTE            NOT NUMERIC)                OR
              (WAFP-CPACOTE            EQUAL ZEROS)                OR
              (WAFP-FIM                NOT EQUAL 'N' AND 'S')
               MOVE 'S'                TO WAFQ-FIM

               MOVE 1                  TO WAFQ-ERRO
               MOVE ZEROS              TO WAFQ-COD-SQL-ERRO
               MOVE 0001               TO WAFQ-COD-MSG-ERRO

               MOVE '0001'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAFQ-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ACESSAR-GFCTB0L0.

           PERFORM 2200-MOVER-DADOS.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-ACESSAR-GFCTB0L0           SECTION.
      *----------------------------------------------------------------*

           MOVE WAFP-CAGPTO            TO CAGPTO-CTA   OF GFCTB0L0.
           MOVE WAFP-CPACOTE           TO CSERVC-TARIF OF GFCTB0L0.
           MOVE WAFP-HINCL             TO HINCL-REG    OF GFCTB0L0.

           EXEC SQL
             SELECT
                     CAGPTO_CTA        ,
                     CSERVC_TARIF      ,
                     HINCL_REG         ,
                     CFUNC_SOLTC_CANCT ,
                     CFUNC_MANUT       ,
                     CSIT_SOLTC_CANCT  ,
                     QPRVIO_CANCT_ADSAO,
                     QADSAO_CANCD      ,
                     HEXCUC_CALC_PRVIO ,
                     HEXCUC_PROCM      ,
                     HMANUT_REG        ,
                     RJUSTF_SOLTC_CANCT,
                     RJUSTF_PAREC_CANCT
               INTO
                    :GFCTB0L0.CAGPTO-CTA        ,
                    :GFCTB0L0.CSERVC-TARIF      ,
                    :GFCTB0L0.HINCL-REG         ,
                    :GFCTB0L0.CFUNC-SOLTC-CANCT ,
                    :GFCTB0L0.CFUNC-MANUT       ,
                    :GFCTB0L0.CSIT-SOLTC-CANCT  ,
                    :GFCTB0L0.QPRVIO-CANCT-ADSAO,
                    :GFCTB0L0.QADSAO-CANCD      ,
                    :GFCTB0L0.HEXCUC-CALC-PRVIO ,
                    :GFCTB0L0.HEXCUC-PROCM      ,
                    :GFCTB0L0.HMANUT-REG        ,
                    :GFCTB0L0.RJUSTF-SOLTC-CANCT,
                    :GFCTB0L0.RJUSTF-PAREC-CANCT
               FROM
                     DB2PRD.TCANCT_ADSAO_AGPTO
              WHERE
                     CAGPTO_CTA         = :GFCTB0L0.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L0.CSERVC-TARIF
                AND  HINCL_REG          = :GFCTB0L0.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL

               MOVE 0006               TO WAFQ-COD-MSG-ERRO

               MOVE '0006'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFQ-DADOS-REST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-MOVER-DADOS                SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-ACESSAR-GFCTB0F3.

           MOVE RAGPTO-CTA OF GFCTB0F3 TO WAFQ-RAGPTO.

           PERFORM 2220-ACESSAR-GFCTB0D8.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8 (1:35)
                                       TO WAFQ-RPACOTE.

           EVALUATE WAFP-CAGPTO
              WHEN 15
              WHEN 17
              WHEN 20

                   PERFORM 2230-OBTER-PAB

              WHEN 16

                   PERFORM 2240-OBTER-EMPRESA

              WHEN 22

                   PERFORM 2250-OBTER-FORNEC

           END-EVALUATE.

           MOVE HINCL-REG OF GFCTB0L0  TO WAFQ-HINCL
                                          WRK-TIMESTAMP.

           MOVE WRK-ANO-TIMESTAMP      TO WRK-ANO-DB2.
           MOVE WRK-MES-TIMESTAMP      TO WRK-MES-DB2.
           MOVE WRK-DIA-TIMESTAMP      TO WRK-DIA-DB2.
           MOVE WRK-DATA-DB2           TO WAFQ-DATA-SOLIC.

           MOVE RJUSTF-SOLTC-CANCT-TEXT OF GFCTB0L0 (1:235)
                                       TO WAFQ-COMENT.

           MOVE CFUNC-SOLTC-CANCT OF GFCTB0L0
                                       TO WRK-CFUNC.
           MOVE WRK-COD-FUNC-TAM-7     TO WRK-COD-FUNC-NUM.
           MOVE WRK-COD-FUNC-X         TO WAFQ-CFUNC-SOLIC.

           PERFORM 2270-OBTER-NOME-FUNC.

           MOVE WRK-NOME-FUNC          TO WAFQ-RFUNC-SOLIC.

           MOVE WRK-DATA-DB2           TO WAFQ-DATA-INCL.

           MOVE WRK-HOR-TIMESTAMP      TO WRK-HOR-AUX.
           MOVE WRK-MIN-TIMESTAMP      TO WRK-MIN-AUX.
           MOVE WRK-HORA-AUX           TO WAFQ-HORA-INCL.

           MOVE QPRVIO-CANCT-ADSAO OF GFCTB0L0
                                       TO WRK-QPRVIO-CANCT-ADSAO.
           MOVE WRK-QTD-PREVIA         TO WAFQ-QTD-PREVIA.

           MOVE QADSAO-CANCD OF GFCTB0L0
                                       TO WRK-QADSAO-CANCD.
           MOVE WRK-QTD-EXCLUIDAS      TO WAFQ-QTD-EXCLUIDAS.

           IF  CFUNC-MANUT OF GFCTB0L0 NOT EQUAL ZEROS
               MOVE CFUNC-MANUT OF GFCTB0L0
                                       TO WRK-CFUNC
               MOVE WRK-COD-FUNC-TAM-7 TO WRK-COD-FUNC-NUM
               MOVE WRK-COD-FUNC-X     TO WAFQ-CFUNC-CANC-PAREC

               PERFORM 2270-OBTER-NOME-FUNC

               MOVE WRK-NOME-FUNC      TO WAFQ-RFUNC-CANC-PAREC
           ELSE
               MOVE ZEROS              TO WAFQ-CFUNC-CANC-PAREC
               MOVE SPACES             TO WAFQ-RFUNC-CANC-PAREC
           END-IF.

           MOVE HMANUT-REG OF GFCTB0L0 TO WRK-TIMESTAMP.

           MOVE WRK-ANO-TIMESTAMP      TO WRK-ANO-DB2.

           MOVE WRK-MES-TIMESTAMP      TO WRK-MES-DB2.
           MOVE WRK-DIA-TIMESTAMP      TO WRK-DIA-DB2.
           MOVE WRK-DATA-DB2           TO WAFQ-DATA-CANC-PAREC.
           MOVE WRK-HOR-TIMESTAMP      TO WRK-HOR-AUX.
           MOVE WRK-MIN-TIMESTAMP      TO WRK-MIN-AUX.
           MOVE WRK-HORA-AUX           TO WAFQ-HORA-CANC-PAREC.

           MOVE CSIT-SOLTC-CANCT OF GFCTB0L0
                                       TO WAFQ-SITUACAO.

           IF  CSIT-SOLTC-CANCT OF GFCTB0L0
                                       EQUAL 03
               MOVE 2                  TO WAFQ-PARECER
           ELSE
               IF  CSIT-SOLTC-CANCT OF GFCTB0L0
                                       EQUAL 04 OR
                   CSIT-SOLTC-CANCT OF GFCTB0L0
                                       EQUAL 05
                   MOVE 1              TO WAFQ-PARECER
               ELSE
                   MOVE 0              TO WAFQ-PARECER
               END-IF
           END-IF.

           MOVE RJUSTF-PAREC-CANCT-TEXT OF GFCTB0L0 (1:235)
                                       TO WAFQ-JUSTIF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-ACESSAR-GFCTB0F3           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA OF GFCTB0F3.

           EXEC SQL
             SELECT
                     RAGPTO_CTA
               INTO
                    :GFCTB0F3.RAGPTO-CTA
               FROM
                     DB2PRD.TPO_AGPTO_CTA
              WHERE
                     CAGPTO_CTA         = :GFCTB0F3.CAGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TPO_AGPTO_CTA'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFQ-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0F3'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFQ-DADOS-REST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-ACESSAR-GFCTB0D8           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0D8.
           EXEC SQL

             SELECT
                     RSERVC_TARIF_REDZD
               INTO
                    :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM
                     DB2PRD.SERVC_TARIF_PRINC
              WHERE
                     CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFQ-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0D8'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFQ-DADOS-REST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2230-OBTER-PAB                  SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA   OF GFCTB0L3.
           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0L3.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG    OF GFCTB0L3.

           EXEC SQL
             SELECT
                     CDEPDC,
                     CPOSTO_SERVC
               INTO
                    :GFCTB0L3.CDEPDC,
                    :GFCTB0L3.CPOSTO-SERVC
               FROM
                     DB2PRD.TCANCT_ADSAO_POSTO
              WHERE
                     CAGPTO_CTA         = :GFCTB0L3.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L3.CSERVC-TARIF
                AND  HINCL_REG          = :GFCTB0L3.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFQ-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L3'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFQ-DADOS-REST

               PERFORM 9999-FINALIZAR

           END-IF.

           MOVE CDEPDC OF GFCTB0L3     TO WRK-AGENCIA.
           MOVE WRK-COD-AGENCIA        TO WAFQ-CAGENCIA.

           PERFORM 2260-OBTER-DESC-AGENCIA.

           MOVE GFCT1X-DESC-DEPDC(1) (1:30)
                                       TO WAFQ-RAGENCIA.

           MOVE CPOSTO-SERVC OF GFCTB0L3
                                       TO WRK-POSTO.
           MOVE WRK-COD-POSTO          TO WAFQ-CPOSTO.

           IF  WAFP-CAGPTO             EQUAL 015
               PERFORM 2231-OBTER-DESC-PAB
           ELSE
               PERFORM 2232-OBTER-DESC-CORRESP-BANC
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)   TO WAFQ-RPOSTO.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2231-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGB-ENTRADA
                                       GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE WAFP-TRANSACAO         TO GFCTGB-TRANSACAO.
           MOVE WAFP-FUNCAO            TO GFCTGB-FUNCAO.
           MOVE WAFP-FUNC-BDSCO        TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE WRK-COD-AGENCIA        TO GFCTGB-COD-DEPDC.
           MOVE WRK-COD-POSTO          TO GFCTGB-COD-POSTO.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFQ-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0147'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAFQ-FIM

               MOVE GFCTGC-ERRO        TO WAFQ-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO WAFQ-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO WAFQ-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE ZEROS          TO WAFQ-COD-SQL-ERRO
                   MOVE 1577           TO WAFQ-COD-MSG-ERRO

                   MOVE '1577'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFQ-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109'
                          ' - '
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFQ-DESC-MSG-ERRO
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2232-OBTER-DESC-CORRESP-BANC    SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGB-ENTRADA
                                       GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE WAFP-TRANSACAO         TO GFCTGB-TRANSACAO.
           MOVE WAFP-FUNCAO            TO GFCTGB-FUNCAO.
           MOVE WAFP-FUNC-BDSCO        TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE WRK-COD-AGENCIA        TO GFCTGB-COD-DEPDC.
           MOVE WRK-COD-POSTO          TO GFCTGB-COD-POSTO.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFQ-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0147'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAFQ-FIM

               MOVE GFCTGC-ERRO        TO WAFQ-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO WAFQ-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO WAFQ-COD-MSG-ERRO


               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE ZEROS          TO WAFQ-COD-SQL-ERRO
                   MOVE 1579           TO WAFQ-COD-MSG-ERRO
                   MOVE '1579'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFQ-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090'
                          ' - '
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFQ-DESC-MSG-ERRO
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2232-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2240-OBTER-EMPRESA              SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA   OF GFCTB0L1.
           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0L1.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG    OF GFCTB0L1.

           EXEC SQL
             SELECT
                     CAG_BCRIA,
                     CCTA_BCRIA_CLI
               INTO
                    :GFCTB0L1.CAG-BCRIA,
                    :GFCTB0L1.CCTA-BCRIA-CLI
               FROM
                     DB2PRD.TCANCT_ADSAO_EMPR
              WHERE
                     CAGPTO_CTA         = :GFCTB0L1.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L1.CSERVC-TARIF
                AND  HINCL_REG          = :GFCTB0L1.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_EMPR'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFQ-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L1'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFQ-DADOS-REST

               PERFORM 9999-FINALIZAR
           END-IF.

           MOVE CAG-BCRIA OF GFCTB0L1  TO WRK-AGENCIA.
           MOVE WRK-COD-AGENCIA        TO WAFQ-CAGENCIA.

           PERFORM 2260-OBTER-DESC-AGENCIA.

           MOVE GFCT1X-DESC-DEPDC(1) (1:30)
                                       TO WAFQ-RAGENCIA.

           MOVE CCTA-BCRIA-CLI OF GFCTB0L1
                                       TO WRK-CONTA.
           MOVE WRK-COD-CONTA          TO WAFQ-CCONTA.

           PERFORM 2241-OBTER-DESC-EMPRESA.

           MOVE GFCTBS-NOME-CLIENTE(1) (1:35)
                                       TO WAFQ-RCONTA.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2241-OBTER-DESC-EMPRESA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTBR-ENTRADA
                                          GFCTBS-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTBR-ENTRADA
                                       GFCTBS-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCTBR-LL.
           MOVE ZEROS                  TO GFCTBR-ZZ.

           MOVE WAFP-TRANSACAO         TO GFCTBR-TRANSACAO.
           MOVE WAFP-FUNCAO            TO GFCTBR-FUNCAO.
           MOVE ZEROS                  TO GFCTBR-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTBR-QTDE-TOT-REG.
           MOVE WAFP-FUNC-BDSCO        TO GFCTBR-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTBR-FIM.

           MOVE WRK-COD-AGENCIA        TO GFCTBR-COD-DEPDC.
           MOVE WRK-COD-CONTA-TAM-7    TO GFCTBR-CCTA-CLI.

           MOVE 'GFCT5060'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTBR-ENTRADA
                                             GFCTBS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFQ-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0147'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF

           IF  GFCTBS-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAFQ-FIM

               MOVE GFCTBS-ERRO        TO WAFQ-ERRO
               MOVE GFCTBS-COD-SQL-ERRO
                                       TO WAFQ-COD-SQL-ERRO
               MOVE GFCTBS-COD-MSG-ERRO
                                       TO WAFQ-COD-MSG-ERRO

               IF  GFCTBS-ERRO         EQUAL 1
                   MOVE ZEROS          TO WAFQ-COD-SQL-ERRO
                   MOVE 1581           TO WAFQ-COD-MSG-ERRO
                   MOVE '1581'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFQ-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5060'
                          ' - '
                          GFCTBS-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFQ-DESC-MSG-ERRO
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2241-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2250-OBTER-FORNEC               SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA   OF GFCTB0L2.
           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0L2.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG    OF GFCTB0L2.

           EXEC SQL
             SELECT
ST2506*              CCNPJ_CPF,
ST2506*              CFLIAL_CNPJ,
ST2506*              CCTRL_CNPJ
ST2506               CCNPJ_CPF_ST,
ST2506               CFLIAL_CNPJ_ST,
ST2506               CCTRL_CNPJ_ST
               INTO
ST2506*             :GFCTB0L2.CCNPJ-CPF,
ST2506*             :GFCTB0L2.CFLIAL-CNPJ,
ST2506*             :GFCTB0L2.CCTRL-CNPJ
ST2506              :GFCTB0L2.CCNPJ-CPF-ST,
ST2506              :GFCTB0L2.CFLIAL-CNPJ-ST,
ST2506              :GFCTB0L2.CCTRL-CNPJ-ST
               FROM
                     DB2PRD.TCANCT_ADSAO_FORNC
              WHERE
                     CAGPTO_CTA         = :GFCTB0L2.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L2.CSERVC-TARIF
                AND  HINCL_REG          = :GFCTB0L2.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_FORNC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFQ-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L2'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFQ-DADOS-REST

               PERFORM 9999-FINALIZAR
           END-IF.

ST2506*    MOVE CCNPJ-CPF OF GFCTB0L2  TO WRK-CCNPJ-CPF.
ST2506*    MOVE WRK-COD-CNPJ           TO WAFQ-CNPJ-NUM.
ST2506     MOVE CCNPJ-CPF-ST           OF GFCTB0L2  
ST2506                                 TO WAFQ-CNPJ-CGC.

ST2506*    MOVE CFLIAL-CNPJ OF GFCTB0L2
ST2506*                                TO WRK-CFLIAL-CNPJ.
ST2506*    MOVE WRK-COD-FILIAL         TO WAFQ-CNPJ-FIL.
ST2506     MOVE CFLIAL-CNPJ-ST         OF GFCTB0L2
                                       TO WAFQ-CNPJ-FIL.

ST2506*    MOVE CCTRL-CNPJ OF GFCTB0L2 TO WRK-CCTRL-CNPJ.
ST2506     MOVE CCTRL-CNPJ-ST OF GFCTB0L2 TO WRK-CCTRL-CNPJ.
           MOVE WRK-COD-CONTROLE       TO WAFQ-CNPJ-CTR.

           PERFORM 2251-OBTER-NOME-FORNEC.


           MOVE ICLI OF PFEBB000 (1:35)
                                       TO WAFQ-RCNPJ.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2251-OBTER-NOME-FORNEC          SECTION.
      *----------------------------------------------------------------*

ST2506*    MOVE WRK-COD-CNPJ           TO CCGC-CPF   OF PFEBB000.
ST2506*    MOVE WRK-COD-FILIAL         TO CFLIAL-CGC OF PFEBB000.
ST2506     MOVE CCNPJ-CPF-ST           OF GFCTB0L2  
ST2506                                 TO CCGC-CPF   OF PFEBB000.
ST2506*    MOVE WRK-COD-FILIAL         TO CFLIAL-CGC OF PFEBB000.
ST2506     MOVE CFLIAL-CNPJ-ST         OF GFCTB0L2
ST2506                                 TO CFLIAL-CGC OF PFEBB000.
           MOVE WRK-COD-CONTROLE       TO CCTRL-CGC  OF PFEBB000.

           EXEC SQL
             SELECT
                     ICLI
               INTO
                    :PFEBB000.ICLI
                     :WRK-ICLI-NULL
               FROM
                     DB2PRD.CLIENTE_PAGADOR
              WHERE
                     CCGC_CPF           = :PFEBB000.CCGC-CPF
                AND  CFLIAL_CGC         = :PFEBB000.CFLIAL-CGC
                AND  CCTRL_CGC          = :PFEBB000.CCTRL-CGC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'CLIENTE_PAGADOR'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFQ-COD-MSG-ERRO


               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'PFEBB000'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFQ-DADOS-REST

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WRK-ICLI-NULL           LESS ZEROS
               MOVE SPACES             TO ICLI OF PFEBB000
           END-IF.

      *----------------------------------------------------------------*
       2251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2260-OBTER-DESC-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT1V-ENTRADA
                                       GFCT1X-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE +660                   TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.

           MOVE WAFP-TRANSACAO         TO GFCT1V-TRANSACAO.
           MOVE WAFP-FUNCAO            TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE WAFP-FUNC-BDSCO        TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE WRK-COD-AGENCIA        TO GFCT1V-COD-DEPDC(1).

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFQ-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0147'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAFQ-FIM

               MOVE GFCT1X-ERRO        TO WAFQ-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO WAFQ-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO WAFQ-COD-MSG-ERRO

               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE ZEROS          TO WAFQ-COD-SQL-ERRO
                   MOVE 1582           TO WAFQ-COD-MSG-ERRO

                   MOVE '1582'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFQ-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006'
                          ' - '
                          GFCT1X-DESC-MSG-ERRO

                   DELIMITED BY SIZE   INTO WAFQ-DESC-MSG-ERRO
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-OBTER-NOME-FUNC            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-ENTRADA
                                          WRK-ERRO-AREA
                                          WRK-SQLCA.

           INITIALIZE                  WRK-ENTRADA
                                       WRK-ERRO-AREA
                                       WRK-SQLCA.


           MOVE WRK-COD-FUNC           TO WRK-PESQUISA-ENT.
           MOVE 0086                   TO WRK-TAMANHO-ENT.
           MOVE 002                    TO WRK-CAMPO-ENT(1).

           MOVE 'SARH8000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-ENTRADA
                                             WRK-SAIDA
                                             WRK-ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-COD-RETORNO-SAI     EQUAL ZEROS
               MOVE WRK-REGISTRO-SAI(1:30)
                                       TO WRK-NOME-FUNC
           ELSE
               IF  WRK-COD-RETORNO-SAI EQUAL 02
                   MOVE WRK-MSG003     TO WRK-NOME-FUNC
               ELSE
                   IF  WRK-COD-RETORNO-SAI
                                       EQUAL 10 OR
                       WRK-COD-RETORNO-SAI
                                       EQUAL 12
                       MOVE ALL '*'    TO WRK-NOME-FUNC
                   ELSE
                       MOVE SPACES     TO WRK-NOME-FUNC
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9900-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO WAFQ-ERRO.
           MOVE 'GFCT0147'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.

           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          WRK-SQLCODE.

           MOVE WRK-COD-SQL            TO WAFQ-COD-SQL-ERRO

           MOVE SPACES                 TO WAFQ-DESC-MSG-ERRO.

           PERFORM 9990-OBTER-DESC-MENSAGEM.

           STRING GFCTG3-DESC-MSG
                  ' - '
                  WRK-COD-TABELA
           DELIMITED BY '  '           INTO WAFQ-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9990-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAFP-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAFP-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAFP-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFQ-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0147'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO WAFQ-ERRO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0147'     TO GFCT0M-TRANSACAO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       9990-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
