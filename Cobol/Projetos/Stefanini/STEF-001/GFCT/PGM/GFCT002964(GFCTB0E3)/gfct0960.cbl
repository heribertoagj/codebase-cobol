      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0960.
       AUTHOR.     ELVYS MARTINS.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0960                                     *
      *    PROGRAMADOR  : ELVYS MARTINS                 - CPM/FPOLIS   *
      *    ANALISTA CPM : ALEXANDRE PEREIRA             - CPM/FPOLIS   *
      *    ANALISTA     : FRANCISCO - PROCWORK          - GRUPO 50     *
      *    DATA         : 13/04/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CONSULTA SOLICITACAO DE LIBERACAO DE SUSPENSAO DE         *
      *      COBRANCA DE TARIFA.                                       *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.SUSP_TARIF_AGPTO                GFCTB0E7          *
      *       DB2PRD.SUSP_GRP_CLI                    GFCTB0E3          *
      *       DB2PRD.SUSP_GRP_DEPDC                  GFCTB0E4          *
      *       DB2PRD.SUSP_GRP_MUN                    GFCTB0E5          *
      *       DB2PRD.SUSP_GRP_UF                     GFCTB0E6          *
      *       DB2PRD.TSUSP_GRP_PAB                   GFCTB0I1          *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0D8          *
      *       DB2PRD.PARM_DATA_PROCM                 GFCTB0A1          *
      *       DB2PRD.V01UF                           DCITV002          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5522 - VERIFICA DISPONIBILIDADE ON-LINE.              *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                  *
      *      MESU9015 - OBTER DESCRICAO DA AGENCIA.                    *
      *      MESU9018 - OBTER DESCRICAO PAB.                           *
      *      RURC9020 - OBTER DESCRICAO MUNICIPIO.                     *
      *      DCIT8000 - OBTER DESCRICAO UF.                            *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                *
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
       01  FILLER                      PIC X(32)         VALUE
           '*  INICIO DA WORKING GFCT0960  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS               PIC 9(03) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-INDEXADORES.
           05  IND-OCOR                PIC 9(03) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)        VALUE SPACES.
           05  WRK-COUNT               PIC S9(09) COMP   VALUE ZEROS.
           05  WRK-FIM-CURSOR          PIC  X(01)        VALUE SPACES.
           05  WRK-QTDE-OCOR           PIC  9(03) COMP-3 VALUE ZEROS.
           05  WRK-QTDE-TOT-REG        PIC  9(06) COMP-3 VALUE ZEROS.
           05  WRK-CDEPDC              PIC S9(05) COMP-3 VALUE ZEROS.
           05  WRK-CPOSTO-SERVC        PIC S9(05) COMP-3 VALUE ZEROS.
           05  WRK-CMUN-IBGE           PIC S9(07) COMP-3 VALUE ZEROS.
           05  WRK-CSGL-UF             PIC  X(02)        VALUE SPACES.
           05  WRK-CSERVC-TARIF        PIC S9(05) COMP-3 VALUE ZEROS.
           05  WRK-COD-TARIFA-INICIO   PIC S9(05) COMP-3 VALUE ZEROS.
           05  WRK-COD-TARIFA-FIM      PIC S9(05) COMP-3 VALUE ZEROS.
           05  WRK-IND-LIBERACAO-INIC  PIC  X(01)        VALUE SPACES.
           05  WRK-IND-LIBERACAO-FIM   PIC  X(01)        VALUE SPACES.
           05  WRK-COD-TABELA          PIC  X(08)        VALUE SPACES.
           05  WRK-NOME-TABELA         PIC  X(18)        VALUE SPACES.
           05  WRK-COMANDO-SQL         PIC  X(10)        VALUE SPACES.
           05  WRK-LOCAL-ERRO          PIC  X(04)        VALUE SPACES.
           05  WRK-NOME-DEPDC-40.
             10  WRK-NOME-DEPDC-20     PIC  X(20)        VALUE SPACES.
             10  FILLER                PIC  X(20)        VALUE SPACES.
           05  WRK-NOME-MUNIC-30.
             10  WRK-NOME-MUNIC-20     PIC  X(20)        VALUE SPACES.
             10  FILLER                PIC  X(10)        VALUE SPACES.
           05  WRK-UF-25.
             10  WRK-UF-20             PIC  X(20)        VALUE SPACES.
             10  FILLER                PIC  X(05)        VALUE SPACES.
           05  WRK-DESC-TARIFA-100.
             10  WRK-DESC-TARIFA-20    PIC  X(20)        VALUE SPACES.
             10  FILLER                PIC  X(80)        VALUE SPACES.
           05  WRK-COD-MUN-9           PIC  9(09)        VALUE ZEROS.
           05  FILLER                  REDEFINES         WRK-COD-MUN-9.
             10  FILLER                PIC  9(02).
             10  WRK-COD-MUN-7         PIC  9(07).

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*     MENSAGENS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05  WRK-MSG01.
             10  FILLER                PIC X(26)         VALUE
                 'ERRO NA CHAMADA DO MODULO '.
             10  WRK-MODULO-MSG        PIC X(08)         VALUE SPACES.
             10  FILLER                PIC X(41)         VALUE SPACES.
           05  WRK-MSG02               PIC X(75)         VALUE
               'SISTEMA INDISPONIVEL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*    AREA DO MODULO DCIT8000   *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT8000-UNID-FEDERATIVA.
           05  WRK-DCIT8000-AREA.
             10  WRK-DCIT8000-DCLGEN-DCITV002
                                       PIC X(70)        VALUE SPACES.
           05  WRK-DCIT8000-CD-RETORNO PIC 9(02)        VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*    AREA DO MODULO GFCT5522   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.
       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*    AREA DO MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.
       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*    AREA DO MODULO MESU9015   *'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*    AREA DO MODULO MESU9018   *'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM8'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*    AREA DO MODULO RURC9020   *'.
      *----------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*           TABELAS DB2        *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0E7
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0E3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0E4
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0E5
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0E6
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0I1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
                INCLUDE DCITV002
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*             CURSORES         *'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE CSR01-GFCTB0E7 CURSOR FOR

               SELECT T1.CSERVC_TARIF,
                      T1.DINIC_SUSP_COBR,
                      T1.CSEQ_AGPTO_CTA,
                      T1.DFIM_SUSP_COBR,
                      T1.CINDCD_LIBRC_COBR,
                      T1.DCOBR_SUSP_LIBRD,
                      T2.CCGC_CPF,
                      T2.CFLIAL_CGC,
                      T2.CCTRL_CPF_CGC,
ST25X6                T2.CCGC_CPF_ST,
ST25X6                T2.CFLIAL_CGC_ST,
ST25X6                T2.CCTRL_CPF_CGC_ST
               FROM   DB2PRD.SUSP_TARIF_AGPTO T1,
                      DB2PRD.SUSP_GRP_CLI     T2

               WHERE (T1.CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                 AND (T1.CSERVC_TARIF      >= :WRK-COD-TARIFA-INICIO
                 AND  T1.CSERVC_TARIF      <= :WRK-COD-TARIFA-FIM)
                 AND (T1.CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INIC
                 AND  T1.CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FIM)
                 AND  T1.DFIM_SUSP_COBR    <= :GFCTB0E7.DFIM-SUSP-COBR
                 AND  T1.CSERVC_TARIF       = T2.CSERVC_TARIF
                 AND  T1.CAGPTO_CTA         = T2.CAGPTO_CTA
                 AND  T1.DINIC_SUSP_COBR    = T2.DINIC_SUSP_COBR
                 AND  T1.CSEQ_AGPTO_CTA     = T2.CSEQ_AGPTO_CTA)

               ORDER BY T1.CSERVC_TARIF,
                        T2.CCGC_CPF,
                        T2.CFLIAL_CGC,
                        T2.CCTRL_CPF_CGC,
                        T1.DFIM_SUSP_COBR
           END-EXEC.

           EXEC SQL
               DECLARE CSR02-GFCTB0E7 CURSOR FOR

               SELECT T1.CSERVC_TARIF,
                      T1.DINIC_SUSP_COBR,
                      T1.CSEQ_AGPTO_CTA,
                      T1.DFIM_SUSP_COBR,
                      T1.CINDCD_LIBRC_COBR,
                      T1.DCOBR_SUSP_LIBRD,
TESTE                 T1.CINDCD_AGPTO_TOT,
                      T2.CDEPDC

               FROM   DB2PRD.SUSP_TARIF_AGPTO T1,
                      DB2PRD.SUSP_GRP_DEPDC   T2

               WHERE (T1.CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                 AND (T1.CSERVC_TARIF      >= :WRK-COD-TARIFA-INICIO
                 AND  T1.CSERVC_TARIF      <= :WRK-COD-TARIFA-FIM)
                 AND (T1.CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INIC
                 AND  T1.CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FIM)
                 AND  T1.DFIM_SUSP_COBR    <= :GFCTB0E7.DFIM-SUSP-COBR
                 AND  T1.CSERVC_TARIF       = T2.CSERVC_TARIF
                 AND  T1.CAGPTO_CTA         = T2.CAGPTO_CTA
                 AND  T1.DINIC_SUSP_COBR    = T2.DINIC_SUSP_COBR
                 AND  T1.CSEQ_AGPTO_CTA     = T2.CSEQ_AGPTO_CTA)

               ORDER BY T1.CSERVC_TARIF,
                        T2.CDEPDC,
                        T1.DFIM_SUSP_COBR
           END-EXEC.

           EXEC SQL
               DECLARE CSR03-GFCTB0E7 CURSOR FOR

               SELECT T1.CSERVC_TARIF,
                      T1.DINIC_SUSP_COBR,
                      T1.CSEQ_AGPTO_CTA,
                      T1.DFIM_SUSP_COBR,
                      T1.CINDCD_LIBRC_COBR,
                      T1.DCOBR_SUSP_LIBRD,
                      T2.CDEPDC,
                      T2.CPOSTO_SERVC

               FROM   DB2PRD.SUSP_TARIF_AGPTO T1,
                      DB2PRD.TSUSP_GRP_PAB    T2

               WHERE (T1.CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                 AND (T1.CSERVC_TARIF      >= :WRK-COD-TARIFA-INICIO
                 AND  T1.CSERVC_TARIF      <= :WRK-COD-TARIFA-FIM)
                 AND (T1.CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INIC
                 AND  T1.CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FIM)
                 AND  T1.DFIM_SUSP_COBR    <= :GFCTB0E7.DFIM-SUSP-COBR
                 AND  T1.CSERVC_TARIF       =  T2.CSERVC_TARIF
                 AND  T1.CAGPTO_CTA         =  T2.CAGPTO_CTA
                 AND  T1.DINIC_SUSP_COBR    =  T2.DINIC_SUSP_COBR
                 AND  T1.CSEQ_AGPTO_CTA     =  T2.CSEQ_AGPTO_CTA)

               ORDER BY T1.CSERVC_TARIF,
                        T2.CDEPDC,
                        T2.CPOSTO_SERVC,
                        T1.DFIM_SUSP_COBR
           END-EXEC.

           EXEC SQL
               DECLARE CSR04-GFCTB0E7 CURSOR FOR

               SELECT T1.CSERVC_TARIF,
                      T1.DINIC_SUSP_COBR,
                      T1.CSEQ_AGPTO_CTA,
                      T1.DFIM_SUSP_COBR,
                      T1.CINDCD_LIBRC_COBR,
                      T1.DCOBR_SUSP_LIBRD,
                      T2.CMUN_IBGE

               FROM   DB2PRD.SUSP_TARIF_AGPTO T1,
                      DB2PRD.SUSP_GRP_MUN     T2

               WHERE (T1.CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                 AND (T1.CSERVC_TARIF      >= :WRK-COD-TARIFA-INICIO
                 AND  T1.CSERVC_TARIF      <= :WRK-COD-TARIFA-FIM)
                 AND (T1.CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INIC
                 AND  T1.CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FIM)
                 AND  T1.DFIM_SUSP_COBR    <= :GFCTB0E7.DFIM-SUSP-COBR
                 AND  T1.CSERVC_TARIF       = T2.CSERVC_TARIF
                 AND  T1.CAGPTO_CTA         = T2.CAGPTO_CTA
                 AND  T1.DINIC_SUSP_COBR    = T2.DINIC_SUSP_COBR
                 AND  T1.CSEQ_AGPTO_CTA     = T2.CSEQ_AGPTO_CTA)

               ORDER BY T1.CSERVC_TARIF,
                        T2.CMUN_IBGE,
                        T1.DFIM_SUSP_COBR
           END-EXEC.

           EXEC SQL
               DECLARE CSR05-GFCTB0E7 CURSOR FOR

               SELECT T1.CSERVC_TARIF,
                      T1.DINIC_SUSP_COBR,
                      T1.CSEQ_AGPTO_CTA,
                      T1.DFIM_SUSP_COBR,
                      T1.CINDCD_LIBRC_COBR,
                      T1.DCOBR_SUSP_LIBRD,
                      T2.CSGL_UF

               FROM   DB2PRD.SUSP_TARIF_AGPTO T1,
                      DB2PRD.SUSP_GRP_UF      T2

               WHERE (T1.CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                 AND (T1.CSERVC_TARIF      >= :WRK-COD-TARIFA-INICIO
                 AND  T1.CSERVC_TARIF      <= :WRK-COD-TARIFA-FIM)
                 AND (T1.CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INIC
                 AND  T1.CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FIM)
                 AND  T1.DFIM_SUSP_COBR    <= :GFCTB0E7.DFIM-SUSP-COBR
                 AND  T1.CSERVC_TARIF       =  T2.CSERVC_TARIF
                 AND  T1.CAGPTO_CTA         =  T2.CAGPTO_CTA
                 AND  T1.DINIC_SUSP_COBR    =  T2.DINIC_SUSP_COBR
                 AND  T1.CSEQ_AGPTO_CTA     =  T2.CSEQ_AGPTO_CTA)

               ORDER BY T1.CSERVC_TARIF,
                        T2.CSGL_UF,
                        T1.DFIM_SUSP_COBR
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*    FIM DA WORKING GFCT0960   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTWG'.
       COPY 'I#GFCTWH'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTWG-ENTRADA
                                             GFCTWH-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL                                               *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR.

           PERFORM 0800-PROCESSAR.

           PERFORM 0400-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSO INICIAL                                               *
      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTWH-SAIDA.

           INITIALIZE GFCTWH-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE ZEROS                  TO GFCT0M-SQLCODE.

           PERFORM 0200-INICIALIZAR-AREA-SAIDA.

           PERFORM 0500-VERIFICAR-DISPONIBILIDADE.

           PERFORM 0600-CONSISTIR-DADOS-ENTRADA.

           PERFORM 0700-OBTER-DATA-PROCESSAMENTO.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * INICIALIZAR AREA DE SAIDA                                      *
      *----------------------------------------------------------------*
       0200-INICIALIZAR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

           MOVE +240                   TO GFCTWH-LL.
           MOVE GFCTWG-TRANSACAO       TO GFCTWH-TRANSACAO.
           MOVE GFCTWG-FUNCAO          TO GFCTWH-FUNCAO.
           MOVE GFCTWG-FILTRO          TO GFCTWH-FILTRO.
           MOVE GFCTWG-FUNC-BDSCO      TO GFCTWH-FUNC-BDSCO.
           MOVE GFCTWG-QTDE-OCOR       TO GFCTWH-QTDE-OCOR.
           MOVE GFCTWG-QTDE-TOT-REG    TO GFCTWH-QTDE-TOT-REG.

           MOVE 'N'                    TO GFCTWH-FIM.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 0300-OBTER-DESCRICAO.

           MOVE GFCTG3-DESC-MSG        TO GFCTWH-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER DESCRICAO DA MENSAGEM                                    *
      *----------------------------------------------------------------*
       0300-OBTER-DESCRICAO            SECTION.
      *----------------------------------------------------------------*

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTWG-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTWG-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTWG-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE 'GFCT0960'         TO GFCT0M-TRANSACAO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTWH-DESC-MSG-ERRO
               MOVE 9                  TO GFCTWH-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE  GFCTG1-ERRO       TO GFCTWH-ERRO
               MOVE 'GFCT0960'         TO GFCT0M-TRANSACAO

               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTWH-COD-MSG-ERRO
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCTWH-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTWH-DESC-MSG-ERRO
               ELSE
                   MOVE SPACES         TO GFCTWH-DESC-MSG-ERRO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
               END-IF

               PERFORM 0400-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTWH-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTWH-COD-MSG-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTWH-COD-SQL-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FINALIZAR                                                      *
      *----------------------------------------------------------------*
       0400-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VERIFICAR SE SISTEMA DISPONIVEL                                *
      *----------------------------------------------------------------*
       0500-VERIFICAR-DISPONIBILIDADE  SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTFZ-ENTRADA
                                          GFCTG1-SAIDA.

           INITIALIZE GFCTFZ-ENTRADA
                      GFCTG1-SAIDA.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE GFCTWG-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTWG-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTWG-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE 'GFCT0960'         TO GFCT0M-TRANSACAO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTWH-DESC-MSG-ERRO
               MOVE 9                  TO GFCTWH-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               MOVE  GFCTG1-ERRO       TO GFCTWH-ERRO
               MOVE 'GFCT0960'         TO GFCT0M-TRANSACAO

               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTWH-COD-MSG-ERRO
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCTWH-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTWH-DESC-MSG-ERRO
               ELSE
                   MOVE SPACES         TO GFCTWH-DESC-MSG-ERRO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
               END-IF

               PERFORM 0400-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'GFCT0960'         TO GFCT0M-TRANSACAO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE WRK-MSG02          TO GFCT0M-TEXTO
                                          GFCTWH-DESC-MSG-ERRO
               MOVE 9                  TO GFCTWH-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONSISTIR DADOS RECEBIDOS                                      *
      *----------------------------------------------------------------*
       0600-CONSISTIR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*

           IF (GFCTWG-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTWG-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTWG-FL-IND-LIBERACAO EQUAL SPACES OR LOW-VALUES) OR
              (GFCTWG-FL-COD-AGRUPAMENTO
                                       NOT NUMERIC               ) OR
              (GFCTWG-FL-COD-AGRUPAMENTO
                                       EQUAL ZEROS               ) OR
              (GFCTWG-FL-COD-AGRUPAMENTO
                                       NOT EQUAL 04 AND 14 AND 15
                                                    AND 18 AND 19) OR
              (GFCTWG-FL-COD-TARIFA    NOT NUMERIC               ) OR
              (GFCTWG-QTDE-OCOR        NOT NUMERIC               ) OR
              (GFCTWG-QTDE-TOT-REG     NOT NUMERIC               ) OR
              (GFCTWG-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTWG-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 'S'                TO GFCTWH-FIM
               MOVE 1                  TO GFCTWH-ERRO
               MOVE 0000001            TO GFCTWH-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE GFCTG3-DESC-MSG    TO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DATA DE PROCESSAMENTO                                 *
      *----------------------------------------------------------------*
       0700-OBTER-DATA-PROCESSAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
                SELECT
                       DPROCM_ATUAL
                INTO
                      :GFCTB0A1.DPROCM-ATUAL

                FROM   DB2PRD.PARM_DATA_PROCM

                WHERE  CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC

           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS  OR
               SQLWARN0                EQUAL 'W'
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCTB0A1'         TO WRK-COD-TABELA
               MOVE 'PARM_DATA_PROCM'  TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0001'             TO WRK-LOCAL-ERRO

               PERFORM 2000-MONTAR-AREA-COMUM-ERRO

               MOVE 9                  TO GFCTWH-ERRO
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTWH-COD-SQL-ERRO
               MOVE 0012               TO GFCTWH-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE SPACES             TO GFCTWH-DESC-MSG-ERRO

               STRING GFCTG3-DESC-MSG ' - ' WRK-COD-TABELA
                   DELIMITED BY '  ' INTO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAR                                                      *
      *----------------------------------------------------------------*
       0800-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWG-FL-COD-AGRUPAMENTO
                                       TO CAGPTO-CTA OF GFCTB0E7.

           IF  GFCTWG-FL-COD-TARIFA    EQUAL ZEROS
               MOVE ZEROS              TO WRK-COD-TARIFA-INICIO
               MOVE 99999              TO WRK-COD-TARIFA-FIM
           ELSE
               MOVE GFCTWG-FL-COD-TARIFA
                                       TO WRK-COD-TARIFA-INICIO
                                          WRK-COD-TARIFA-FIM
           END-IF.

           IF  GFCTWG-FL-IND-LIBERACAO EQUAL 'T'
               MOVE 'A'                TO WRK-IND-LIBERACAO-INIC
               MOVE 'Z'                TO WRK-IND-LIBERACAO-FIM
           ELSE
               MOVE GFCTWG-FL-IND-LIBERACAO
                                       TO WRK-IND-LIBERACAO-INIC
                                          WRK-IND-LIBERACAO-FIM
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DFIM-SUSP-COBR    OF GFCTB0E7.

           MOVE ZEROS                  TO WRK-CSERVC-TARIF
                                          IND-OCOR.

           MOVE 'N'                    TO WRK-FIM-CURSOR.

           PERFORM 0900-CONTAR-LINHA-GFCTB0E7.

           IF  WRK-COUNT               EQUAL ZEROS
               MOVE 'S'                TO GFCTWH-FIM
               MOVE 1                  TO GFCTWH-ERRO
               MOVE ZERO               TO GFCTWH-COD-SQL-ERRO
               MOVE 1248               TO GFCTWH-COD-MSG-ERRO
               MOVE '1248'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE GFCTG3-DESC-MSG    TO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

           PERFORM 1000-ABRIR-CURSOR-GFCTB0E7.

           PERFORM 1100-LER-CURSOR-GFCTB0E7
               UNTIL ACU-LIDOS         GREATER GFCTWG-QTDE-OCOR OR
                     WRK-FIM-CURSOR    EQUAL 'S'.

           MOVE ZEROS                  TO IND-OCOR.

           PERFORM 1300-PROCESSAR-CSR-GFCTB0E7
               UNTIL WRK-FIM-CURSOR    EQUAL 'S' OR
                     IND-OCOR          GREATER 06.

           PERFORM 1200-FECHAR-CURSOR-GFCTB0E7.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONTAR LINHAS DE GFCTB0E7                                      *
      *----------------------------------------------------------------*
       0900-CONTAR-LINHA-GFCTB0E7      SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTWG-FL-COD-AGRUPAMENTO

             WHEN 04

               EXEC SQL
                  SELECT COUNT(*)
                  INTO  :WRK-COUNT
                  FROM   DB2PRD.SUSP_TARIF_AGPTO T1,
                         DB2PRD.SUSP_GRP_CLI     T2
                  WHERE (T1.CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                    AND (T1.CSERVC_TARIF      >= :WRK-COD-TARIFA-INICIO
                    AND  T1.CSERVC_TARIF      <= :WRK-COD-TARIFA-FIM)
                    AND (T1.CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INIC
                    AND  T1.CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FIM)
                    AND  T1.CSERVC_TARIF       =  T2.CSERVC_TARIF
                    AND  T1.CAGPTO_CTA         =  T2.CAGPTO_CTA
                    AND  T1.DINIC_SUSP_COBR    =  T2.DINIC_SUSP_COBR
                    AND  T1.CSEQ_AGPTO_CTA     =  T2.CSEQ_AGPTO_CTA)
               END-EXEC

             WHEN 14

               EXEC SQL
                  SELECT COUNT(*)
                  INTO  :WRK-COUNT
                  FROM   DB2PRD.SUSP_TARIF_AGPTO T1,
                         DB2PRD.SUSP_GRP_DEPDC   T2
                  WHERE (T1.CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                    AND (T1.CSERVC_TARIF      >= :WRK-COD-TARIFA-INICIO
                    AND  T1.CSERVC_TARIF      <= :WRK-COD-TARIFA-FIM)
                    AND (T1.CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INIC
                    AND  T1.CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FIM)
                    AND  T1.CSERVC_TARIF       =  T2.CSERVC_TARIF
                    AND  T1.CAGPTO_CTA         =  T2.CAGPTO_CTA
                    AND  T1.DINIC_SUSP_COBR    =  T2.DINIC_SUSP_COBR
                    AND  T1.CSEQ_AGPTO_CTA     =  T2.CSEQ_AGPTO_CTA)
               END-EXEC

             WHEN 15

               EXEC SQL
                  SELECT COUNT(*)
                  INTO  :WRK-COUNT
                  FROM   DB2PRD.SUSP_TARIF_AGPTO T1,
                         DB2PRD.TSUSP_GRP_PAB    T2
                  WHERE (T1.CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                    AND (T1.CSERVC_TARIF      >= :WRK-COD-TARIFA-INICIO
                    AND  T1.CSERVC_TARIF      <= :WRK-COD-TARIFA-FIM)
                    AND (T1.CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INIC
                    AND  T1.CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FIM)
                    AND  T1.CSERVC_TARIF       =  T2.CSERVC_TARIF
                    AND  T1.CAGPTO_CTA         =  T2.CAGPTO_CTA
                    AND  T1.DINIC_SUSP_COBR    =  T2.DINIC_SUSP_COBR
                    AND  T1.CSEQ_AGPTO_CTA     =  T2.CSEQ_AGPTO_CTA)
               END-EXEC

             WHEN 18

               EXEC SQL
                  SELECT COUNT(*)
                  INTO  :WRK-COUNT
                  FROM   DB2PRD.SUSP_TARIF_AGPTO T1,
                         DB2PRD.SUSP_GRP_MUN     T2
                  WHERE (T1.CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                    AND (T1.CSERVC_TARIF      >= :WRK-COD-TARIFA-INICIO
                    AND  T1.CSERVC_TARIF      <= :WRK-COD-TARIFA-FIM)
                    AND (T1.CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INIC
                    AND  T1.CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FIM)
                    AND  T1.CSERVC_TARIF       =  T2.CSERVC_TARIF
                    AND  T1.CAGPTO_CTA         =  T2.CAGPTO_CTA
                    AND  T1.DINIC_SUSP_COBR    =  T2.DINIC_SUSP_COBR
                    AND  T1.CSEQ_AGPTO_CTA     =  T2.CSEQ_AGPTO_CTA)
               END-EXEC

             WHEN 19

               EXEC SQL
                  SELECT COUNT(*)
                  INTO  :WRK-COUNT
                  FROM   DB2PRD.SUSP_TARIF_AGPTO T1,
                         DB2PRD.SUSP_GRP_UF      T2
                  WHERE (T1.CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                    AND (T1.CSERVC_TARIF      >= :WRK-COD-TARIFA-INICIO
                    AND  T1.CSERVC_TARIF      <= :WRK-COD-TARIFA-FIM)
                    AND (T1.CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INIC
                    AND  T1.CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FIM)
                    AND  T1.CSERVC_TARIF       =  T2.CSERVC_TARIF
                    AND  T1.CAGPTO_CTA         =  T2.CAGPTO_CTA
                    AND  T1.DINIC_SUSP_COBR    =  T2.DINIC_SUSP_COBR
                    AND  T1.CSEQ_AGPTO_CTA     =  T2.CSEQ_AGPTO_CTA)
               END-EXEC

           END-EVALUATE.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCTB0E7'         TO WRK-COD-TABELA
               MOVE 'SUSP_TARIF_AGPTO' TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0001'             TO WRK-LOCAL-ERRO

               PERFORM 2000-MONTAR-AREA-COMUM-ERRO

               MOVE 9                  TO GFCTWH-ERRO
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTWH-COD-SQL-ERRO
               MOVE 0012               TO GFCTWH-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE SPACES             TO GFCTWH-DESC-MSG-ERRO

               STRING GFCTG3-DESC-MSG ' - ' WRK-COD-TABELA
                   DELIMITED BY '  ' INTO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

           IF  WRK-COUNT               GREATER 150
               MOVE 'S'                TO GFCTWH-FIM
               MOVE 1                  TO GFCTWH-ERRO
               MOVE ZERO               TO GFCTWH-COD-SQL-ERRO
               MOVE 1247               TO GFCTWH-COD-MSG-ERRO
                                          GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE GFCTG3-DESC-MSG    TO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ABRIR CURSOR                                                   *
      *----------------------------------------------------------------*
       1000-ABRIR-CURSOR-GFCTB0E7      SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTWG-FL-COD-AGRUPAMENTO

             WHEN 04

               EXEC SQL
                   OPEN CSR01-GFCTB0E7
               END-EXEC

             WHEN 14

               EXEC SQL
                   OPEN CSR02-GFCTB0E7
               END-EXEC

             WHEN 15

               EXEC SQL
                   OPEN CSR03-GFCTB0E7
               END-EXEC

             WHEN 18

               EXEC SQL
                   OPEN CSR04-GFCTB0E7
               END-EXEC

             WHEN 19

               EXEC SQL
                   OPEN CSR05-GFCTB0E7
               END-EXEC

           END-EVALUATE.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCTB0E7'         TO WRK-COD-TABELA
               MOVE 'SUSP_TARIF_AGPTO' TO WRK-NOME-TABELA
               MOVE 'OPEN'             TO WRK-COMANDO-SQL
               MOVE '0002'             TO WRK-LOCAL-ERRO

               PERFORM 2000-MONTAR-AREA-COMUM-ERRO

               MOVE 9                  TO GFCTWH-ERRO
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTWH-COD-SQL-ERRO
               MOVE 0005               TO GFCTWH-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE SPACES             TO GFCTWH-DESC-MSG-ERRO

               STRING GFCTG3-DESC-MSG ' - ' WRK-COD-TABELA
                   DELIMITED BY '  ' INTO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * LER CURSOR                                                     *
      *----------------------------------------------------------------*
       1100-LER-CURSOR-GFCTB0E7        SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTWG-FL-COD-AGRUPAMENTO

             WHEN 04

               EXEC SQL
                   FETCH CSR01-GFCTB0E7 INTO
                       :GFCTB0E7.CSERVC-TARIF,
                       :GFCTB0E7.DINIC-SUSP-COBR,
                       :GFCTB0E7.CSEQ-AGPTO-CTA,
                       :GFCTB0E7.DFIM-SUSP-COBR,
                       :GFCTB0E7.CINDCD-LIBRC-COBR,
                       :GFCTB0E7.DCOBR-SUSP-LIBRD,
                       :GFCTB0E3.CCGC-CPF,
                       :GFCTB0E3.CFLIAL-CGC,
                       :GFCTB0E3.CCTRL-CPF-CGC,
ST25X6                 :GFCTB0E3.CCGC-CPF-ST,
ST25X6                 :GFCTB0E3.CFLIAL-CGC-ST,
ST25X6                 :GFCTB0E3.CCTRL-CPF-CGC-ST
               END-EXEC

             WHEN 14

               EXEC SQL
                   FETCH CSR02-GFCTB0E7 INTO
                       :GFCTB0E7.CSERVC-TARIF,
                       :GFCTB0E7.DINIC-SUSP-COBR,
                       :GFCTB0E7.CSEQ-AGPTO-CTA,
                       :GFCTB0E7.DFIM-SUSP-COBR,
                       :GFCTB0E7.CINDCD-LIBRC-COBR,
                       :GFCTB0E7.DCOBR-SUSP-LIBRD,
TESTE                  :GFCTB0E7.CINDCD-AGPTO-TOT,
                       :GFCTB0E4.CDEPDC
               END-EXEC

             WHEN 15

               EXEC SQL
                   FETCH CSR03-GFCTB0E7 INTO
                       :GFCTB0E7.CSERVC-TARIF,
                       :GFCTB0E7.DINIC-SUSP-COBR,
                       :GFCTB0E7.CSEQ-AGPTO-CTA,
                       :GFCTB0E7.DFIM-SUSP-COBR,
                       :GFCTB0E7.CINDCD-LIBRC-COBR,
                       :GFCTB0E7.DCOBR-SUSP-LIBRD,
                       :GFCTB0I1.CDEPDC,
                       :GFCTB0I1.CPOSTO-SERVC
               END-EXEC

             WHEN 18

               EXEC SQL
                   FETCH CSR04-GFCTB0E7 INTO
                       :GFCTB0E7.CSERVC-TARIF,
                       :GFCTB0E7.DINIC-SUSP-COBR,
                       :GFCTB0E7.CSEQ-AGPTO-CTA,
                       :GFCTB0E7.DFIM-SUSP-COBR,
                       :GFCTB0E7.CINDCD-LIBRC-COBR,
                       :GFCTB0E7.DCOBR-SUSP-LIBRD,
                       :GFCTB0E5.CMUN-IBGE
               END-EXEC

             WHEN 19

               EXEC SQL
                   FETCH CSR05-GFCTB0E7 INTO
                       :GFCTB0E7.CSERVC-TARIF,
                       :GFCTB0E7.DINIC-SUSP-COBR,
                       :GFCTB0E7.CSEQ-AGPTO-CTA,
                       :GFCTB0E7.DFIM-SUSP-COBR,
                       :GFCTB0E7.CINDCD-LIBRC-COBR,
                       :GFCTB0E7.DCOBR-SUSP-LIBRD,
                       :GFCTB0E6.CSGL-UF
               END-EXEC

           END-EVALUATE.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCTB0E7'         TO WRK-COD-TABELA
               MOVE 'SUSP_TARIF_AGPTO' TO WRK-NOME-TABELA
               MOVE 'FETCH'            TO WRK-COMANDO-SQL
               MOVE '0003'             TO WRK-LOCAL-ERRO

               PERFORM 2000-MONTAR-AREA-COMUM-ERRO

               MOVE 9                  TO GFCTWH-ERRO
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTWH-COD-SQL-ERRO
               MOVE 0005               TO GFCTWH-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE SPACES             TO GFCTWH-DESC-MSG-ERRO

               STRING GFCTG3-DESC-MSG ' - ' WRK-COD-TABELA
                   DELIMITED BY '  ' INTO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 'S'                TO WRK-FIM-CURSOR
                                          GFCTWH-FIM
           ELSE
               ADD 1                   TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FECHAR CURSOR                                                  *
      *----------------------------------------------------------------*
       1200-FECHAR-CURSOR-GFCTB0E7     SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTWG-FL-COD-AGRUPAMENTO

             WHEN 04

               EXEC SQL
                   CLOSE CSR01-GFCTB0E7
               END-EXEC

             WHEN 14

               EXEC SQL
                   CLOSE CSR02-GFCTB0E7
               END-EXEC

             WHEN 15

               EXEC SQL
                   CLOSE CSR03-GFCTB0E7
               END-EXEC

             WHEN 18

               EXEC SQL
                   CLOSE CSR04-GFCTB0E7
               END-EXEC

             WHEN 19

               EXEC SQL
                   CLOSE CSR05-GFCTB0E7
               END-EXEC

           END-EVALUATE.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCTB0E7'         TO WRK-COD-TABELA
               MOVE 'SUSP_TARIF_AGPTO' TO WRK-NOME-TABELA
               MOVE 'CLOSE'            TO WRK-COMANDO-SQL
               MOVE '0005'             TO WRK-LOCAL-ERRO

               PERFORM 2000-MONTAR-AREA-COMUM-ERRO

               MOVE 9                  TO GFCTWH-ERRO
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTWH-COD-SQL-ERRO
               MOVE 0011               TO GFCTWH-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE SPACES             TO GFCTWH-DESC-MSG-ERRO

               STRING GFCTG3-DESC-MSG ' - ' WRK-COD-TABELA
                   DELIMITED BY '  ' INTO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAR CURSOR                                               *
      *----------------------------------------------------------------*
       1300-PROCESSAR-CSR-GFCTB0E7     SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTWG-FL-COD-AGRUPAMENTO

             WHEN 14

TESTE          IF  CINDCD-AGPTO-TOT OF GFCTB0E7
TESTE                                  EQUAL  'S'
TESTE              MOVE 'TODAS'        TO COMU-NOME-DEPDC
TESTE
TESTE              MOVE ZEROS          TO WRK-CDEPDC
TESTE          ELSE
                   IF  CDEPDC OF GFCTB0E4
                                       NOT EQUAL WRK-CDEPDC
                       PERFORM 1400-OBTER-NOME-AGENCIA

                       MOVE CDEPDC OF GFCTB0E4
                                       TO WRK-CDEPDC
                   END-IF
END-IF         END-IF

             WHEN 15

               IF  CDEPDC OF GFCTB0I1  NOT EQUAL WRK-CDEPDC OR
                   CPOSTO-SERVC OF GFCTB0I1
                                       NOT EQUAL WRK-CPOSTO-SERVC

                   PERFORM 1500-OBTER-NOME-POSTO

                   MOVE CDEPDC OF GFCTB0I1
                                       TO WRK-CDEPDC
                   MOVE CPOSTO-SERVC OF GFCTB0I1
                                       TO WRK-CPOSTO-SERVC
               END-IF

             WHEN 18

               IF  CMUN-IBGE OF GFCTB0E5
                                       NOT EQUAL WRK-CMUN-IBGE
                   PERFORM 1600-OBTER-NOME-MUNICIPIO

                   MOVE CMUN-IBGE OF GFCTB0E5
                                       TO WRK-CMUN-IBGE
               END-IF

             WHEN 19

               IF  CSGL-UF OF GFCTB0E6 NOT EQUAL WRK-CSGL-UF
                   PERFORM 1700-OBTER-NOME-UNID-FEDE

                   MOVE CSGL-UF OF GFCTB0E6
                                       TO WRK-CSGL-UF
               END-IF

           END-EVALUATE.

           IF  CSERVC-TARIF OF GFCTB0E7
                                       NOT EQUAL WRK-CSERVC-TARIF
               PERFORM 1800-OBTER-DESCRICAO-TARIFA

               MOVE CSERVC-TARIF OF GFCTB0E7
                                       TO WRK-CSERVC-TARIF
           END-IF.

           PERFORM 1900-MONTAR-AREA-SAIDA.

           PERFORM 1100-LER-CURSOR-GFCTB0E7.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER NOME AGENCIA                                             *
      *----------------------------------------------------------------*
       1400-OBTER-NOME-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-PARM.

           MOVE SPACES                 TO WRK-SQLCA
                                          WRK-POOL7100.
           MOVE 237                    TO COMU-BANCO.
           MOVE CDEPDC OF GFCTB0E4     TO COMU-DEPDC.
           MOVE 'GFCT0960'             TO COMU-NOME-PGM.

           MOVE 'MESU9015'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             COMU-PARM
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  COMU-COD-RETORNO        NOT EQUAL ZEROS
               PERFORM 1200-FECHAR-CURSOR-GFCTB0E7

               MOVE 'S'                TO GFCTWH-FIM

               MOVE 1                  TO GFCTWH-ERRO
               MOVE ZEROS              TO GFCTWH-COD-SQL-ERRO
               MOVE 0465               TO GFCTWH-COD-MSG-ERRO
               MOVE '0465'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE GFCTG3-DESC-MSG    TO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER NOME POSTO                                               *
      *----------------------------------------------------------------*
       1500-OBTER-NOME-POSTO           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-DADOS-9018.

           MOVE 'VRS001'               TO COMU-VERSAO.
           MOVE 237                    TO COMU-COD-EMPR.
           MOVE CDEPDC OF GFCTB0I1     TO COMU-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB0I1
                                       TO COMU-COD-POSTO.
           MOVE SPACES                 TO COMU-CORR-BANCARIO.

           MOVE 'MESU9018'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             COMU-DADOS-9018.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               EVALUATE RETURN-CODE

                   WHEN 4

                       MOVE 0518       TO GFCTWH-COD-MSG-ERRO
                       MOVE '0518'     TO GFCTG2-COD-MSG

                   WHEN 8

                       MOVE 0155       TO GFCTWH-COD-MSG-ERRO
                       MOVE '0155'     TO GFCTG2-COD-MSG

                   WHEN 12

                       MOVE 0520       TO GFCTWH-COD-MSG-ERRO
                       MOVE '0520'     TO GFCTG2-COD-MSG

               END-EVALUATE

               PERFORM 1200-FECHAR-CURSOR-GFCTB0E7

               MOVE 'S'                TO GFCTWH-FIM

               MOVE 1                  TO GFCTWH-ERRO
               MOVE ZEROS              TO GFCTWH-COD-SQL-ERRO

               PERFORM 0300-OBTER-DESCRICAO

               MOVE GFCTG3-DESC-MSG    TO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

           IF  COMU-COD-TIPO-POSTO     EQUAL 9
               PERFORM 1200-FECHAR-CURSOR-GFCTB0E7

               MOVE 'S'                TO GFCTWH-FIM

               MOVE 1                  TO GFCTWH-ERRO
               MOVE ZEROS              TO GFCTWH-COD-SQL-ERRO
               MOVE 0594               TO GFCTWH-COD-MSG-ERRO
               MOVE '0594'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE GFCTG3-DESC-MSG    TO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER NOME MUNICIPIO                                           *
      *----------------------------------------------------------------*
       1600-OBTER-NOME-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE OF GFCTB0E5  TO WRK-COD-MUN-7.
           MOVE WRK-COD-MUN-9          TO COD-MUNIC-RURC88.

           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 1200-FECHAR-CURSOR-GFCTB0E7

               MOVE 'S'                TO GFCTWH-FIM

               MOVE 1                  TO GFCTWH-ERRO
               MOVE ZEROS              TO GFCTWH-COD-SQL-ERRO
               MOVE 0732               TO GFCTWH-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE GFCTG3-DESC-MSG    TO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      NOT EQUAL ZEROS
               EVALUATE COD-RETORNO-RURC88

                   WHEN 99

                       MOVE 0734       TO GFCTWH-COD-MSG-ERRO
                       MOVE '0734'     TO GFCTG2-COD-MSG

                   WHEN 20

                       MOVE 0733       TO GFCTWH-COD-MSG-ERRO
                       MOVE '0733'     TO GFCTG2-COD-MSG

                   WHEN OTHER

                       MOVE 0675       TO GFCTWH-COD-MSG-ERRO
                       MOVE '0675'     TO GFCTG2-COD-MSG

               END-EVALUATE

               PERFORM 1200-FECHAR-CURSOR-GFCTB0E7

               MOVE 'S'                TO GFCTWH-FIM

               MOVE 1                  TO GFCTWH-ERRO
               MOVE ZEROS              TO GFCTWH-COD-SQL-ERRO

               PERFORM 0300-OBTER-DESCRICAO

               MOVE GFCTG3-DESC-MSG    TO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER NOME UNIDADE FEDERATIVA                                  *
      *----------------------------------------------------------------*
       1700-OBTER-NOME-UNID-FEDE       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-DCIT8000-UNID-FEDERATIVA
                                       WRK-POOL7100
                                       WRK-SQLCA.

           MOVE CSGL-UF OF GFCTB0E6    TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-DCIT8000-DCLGEN-DCITV002.

           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL 'POOL0081'            USING WRK-MODULO
                                            WRK-DCIT8000-UNID-FEDERATIVA
                                            WRK-POOL7100
                                            WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS OR
               WRK-DCIT8000-CD-RETORNO EQUAL 04
               MOVE ZEROS              TO RETURN-CODE
               MOVE 'GFCT0960'         TO GFCT0M-TRANSACAO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 9                  TO GFCTWH-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

           IF  WRK-DCIT8000-CD-RETORNO NOT EQUAL ZEROS
               EVALUATE WRK-DCIT8000-CD-RETORNO

                   WHEN 01

                       MOVE 0736       TO GFCTWH-COD-MSG-ERRO
                       MOVE '0736'     TO GFCTG2-COD-MSG

                   WHEN 02

                       MOVE 0737       TO GFCTWH-COD-MSG-ERRO
                       MOVE '0737'     TO GFCTG2-COD-MSG

                   WHEN 03

                       MOVE 0738       TO GFCTWH-COD-MSG-ERRO
                       MOVE '0738'     TO GFCTG2-COD-MSG

               END-EVALUATE

               PERFORM 1200-FECHAR-CURSOR-GFCTB0E7

               MOVE 'S'                TO GFCTWH-FIM

               MOVE 1                  TO GFCTWH-ERRO
               MOVE ZEROS              TO GFCTWH-COD-SQL-ERRO

               PERFORM 0300-OBTER-DESCRICAO

               MOVE GFCTG3-DESC-MSG    TO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

           MOVE WRK-DCIT8000-DCLGEN-DCITV002
                                       TO DCITV002.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER DESCRICAO TARIFA                                         *
      *----------------------------------------------------------------*
       1800-OBTER-DESCRICAO-TARIFA     SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0E7
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT RSERVC_TARIF_REDZD
               INTO  :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM   DB2PRD.SERVC_TARIF_PRINC
               WHERE  CSERVC_TARIF = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               PERFORM 1200-FECHAR-CURSOR-GFCTB0E7
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCTB0D8'         TO WRK-COD-TABELA
               MOVE 'SERVC_TARIF_PRINC'
                                       TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0004'             TO WRK-LOCAL-ERRO

               PERFORM 2000-MONTAR-AREA-COMUM-ERRO

               MOVE 9                  TO GFCTWH-ERRO
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTWH-COD-SQL-ERRO
               MOVE 0012               TO GFCTWH-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               PERFORM 0300-OBTER-DESCRICAO

               MOVE SPACES             TO GFCTWH-DESC-MSG-ERRO

               STRING GFCTG3-DESC-MSG ' - ' WRK-COD-TABELA
                   DELIMITED BY '  ' INTO GFCTWH-DESC-MSG-ERRO

               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * MONTAR AREA DE SAIDA                                           *
      *----------------------------------------------------------------*
       1900-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           ADD +110                    TO GFCTWH-LL.
           ADD 1                       TO IND-OCOR.

           ADD 1                       TO GFCTWH-QTDE-OCOR.
           ADD 1                       TO GFCTWH-QTDE-TOT-REG.

           EVALUATE GFCTWG-FL-COD-AGRUPAMENTO

             WHEN 04

ST25X6*        MOVE CCGC-CPF      OF GFCTB0E3
ST25X6*                                TO GFCTWH-COD-CNPJ-CPF(IND-OCOR)
ST25X6         MOVE CCGC-CPF-ST   OF GFCTB0E3
ST25X6                                 TO GFCTWH-COD-CNPJ-CPF(IND-OCOR)
ST25X6*        MOVE CFLIAL-CGC    OF GFCTB0E3
ST25X6*                                TO GFCTWH-NUM-FILIAL  (IND-OCOR)
ST25X6         MOVE CFLIAL-CGC-ST OF GFCTB0E3
ST25X6                                 TO GFCTWH-NUM-FILIAL  (IND-OCOR)
ST25X6*        MOVE CCTRL-CPF-CGC OF GFCTB0E3
ST25X6*                                TO GFCTWH-COD-CONTROLE(IND-OCOR)
ST25X6         MOVE CCTRL-CPF-CGC-ST OF GFCTB0E3
ST25X6                                 TO GFCTWH-COD-CONTROLE(IND-OCOR)

             WHEN 14

               MOVE CDEPDC OF GFCTB0E4 TO GFCTWH-COD-AGENCIA (IND-OCOR)
               MOVE COMU-NOME-DEPDC    TO WRK-NOME-DEPDC-40
               MOVE WRK-NOME-DEPDC-20  TO GFCTWH-DESC-COMPONENTE
                                                             (IND-OCOR)

             WHEN 15

               MOVE CDEPDC       OF GFCTB0I1
                                       TO GFCTWH-COD-AGENCIA (IND-OCOR)
               MOVE CPOSTO-SERVC OF GFCTB0I1
                                       TO GFCTWH-COD-POSTO   (IND-OCOR)
               MOVE COMU-NOME-POSTO    TO GFCTWH-DESC-COMPONENTE
                                                             (IND-OCOR)

             WHEN 18

               MOVE CMUN-IBGE OF GFCTB0E5
                                       TO GFCTWH-COD-MUNICIPIO
                                                             (IND-OCOR)
               MOVE NOME-MUNIC-RURC88  TO WRK-NOME-MUNIC-30
               MOVE WRK-NOME-MUNIC-20  TO GFCTWH-DESC-COMPONENTE
                                                             (IND-OCOR)

             WHEN 19

               MOVE CSGL-UF OF GFCTB0E6
                                       TO GFCTWH-COD-UF      (IND-OCOR)
               MOVE IUF OF DCITV002    TO WRK-UF-25
               MOVE WRK-UF-20          TO GFCTWH-DESC-COMPONENTE
                                                             (IND-OCOR)

           END-EVALUATE.

           MOVE DINIC-SUSP-COBR    OF GFCTB0E7
                                       TO GFCTWH-DATA-INIC-SUSP
                                                             (IND-OCOR).
           MOVE CSEQ-AGPTO-CTA     OF GFCTB0E7
                                       TO GFCTWH-SEQ-AGRUPAMENTO
                                                             (IND-OCOR).
           MOVE CSERVC-TARIF       OF GFCTB0E7
                                       TO GFCTWH-COD-TARIFA  (IND-OCOR).
           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO WRK-DESC-TARIFA-100
           MOVE WRK-DESC-TARIFA-20     TO GFCTWH-DESC-TARIFA (IND-OCOR).
           MOVE DFIM-SUSP-COBR     OF GFCTB0E7
                                       TO GFCTWH-DATA-FIM-SUSP
                                                             (IND-OCOR).
           MOVE DCOBR-SUSP-LIBRD   OF GFCTB0E7
                                       TO GFCTWH-DATA-COBRANCA
                                                             (IND-OCOR).
           MOVE CINDCD-LIBRC-COBR  OF GFCTB0E7
                                       TO GFCTWH-COD-SITUACAO
                                                             (IND-OCOR).

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * MONTAR AREA COMUM DE ERRO                                      *
      *----------------------------------------------------------------*
       2000-MONTAR-AREA-COMUM-ERRO     SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0960'             TO GFCT0M-TRANSACAO
                                          GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE WRK-NOME-TABELA        TO GFCT0M-NOME-TAB.
           MOVE WRK-COMANDO-SQL        TO GFCT0M-COMANDO-SQL.
           MOVE SQLCODE                TO GFCT0M-SQLCODE.
           MOVE WRK-LOCAL-ERRO         TO GFCT0M-LOCAL.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
