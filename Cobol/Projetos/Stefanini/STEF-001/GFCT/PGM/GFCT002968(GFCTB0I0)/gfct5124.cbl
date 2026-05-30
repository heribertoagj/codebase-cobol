      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5124.
       AUTHOR.     SIMONI FAVRETTO.
      *================================================================*
      *                    C P M  -  S I S T E M A S.                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5124                                    *
      *    PROGRAMADORA:   SIMONI FAVRETTO         - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO LOPES SANT'ANA  - CPM PATO BRANCO   *
      *    ANALISTA....:   FRANCISCO               - PROCWORK /GP.50   *
      *    DATA........:   07/04/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA COMBO DE TARIFAS PACOTE    *
      *                    VIGENTES E FUTURAS.                         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                     INCLUDE/BOOK        *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *                   DB2PRD.PARM_SERVC_TARIF    GFCTB0A2          *
      *                   DB2PRD.PRMSS_ADSAO_PCOTE   GFCTB0C6          *
      *                   DB2PRD.PRMSS_GRP_DEPDC     GFCTB0C7          *
      *                   DB2PRD.PRMSS_GRP_EMPR      GFCTB0C8          *
      *                   DB2PRD.PRMSS_GRP_MUN       GFCTB0C9          *
      *                   DB2PRD.PRMSS_GRP_PAB       GFCTB0D0          *
      *                   DB2PRD.PRMSS_GRP_PSTAL     GFCTB0D1          *
      *                   DB2PRD.PRMSS_GRP_SGMTO     GFCTB0D2          *
      *                   DB2PRD.PRMSS_GRP_UF        GFCTB0D3          *
      *                   DB2PRD.SERVC_TARIF_PRINC   GFCTB0D8          *
      *                   DB2PRD.TPRMSS_GRP_CLI      GFCTB0I0          *
011209*                   DB2PRD.TCAD_CORSP_BCRIO    YMKCB001          *
BI0511*                   DB2PRD.PARM_SERVC_TARIF    GFCTB0A2          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTLD - ENTRADA - TARIFAS PACOTE VIGENTES E FUTURAS.     *
      *    I#GFCTLE - SAIDA   - TARIFAS PACOTE VIGENTES E FUTURAS.     *
      *    I#GFCTJM - ENTRADA E SAIDA - OBTER COD SEG GESTAO TARIFA.   *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#GFCTRK - ENTRADA E SAIDA - OBTER COD UF E MUNICIPIO DA    *
      *               AGENCIA.                                         *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5527 - OBTER CPF/CNPJ/POSTO DE SERVICO.                 *
      *    GFCT5534 - OBTER CODIGO SEGMENTO GESTAO TARIFA.             *
      *    GFCT5564 - OBTER COD DA UF E MUNICIPIO DA AGENCIA.          *
011209*    MESU9018 - CONSISTIR PAB.                                   *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *                                                                *
2011  *----------------------------------------------------------------*
2011  *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
2011  *----------------------------------------------------------------*
2011  *    ULTIMA ALTERACAO EM 20/11/2009 - HELIO                      *
2011  *    - INCLUIR INDICADOR DE TAIFA CELULAR :                      *
2011  *             0 - PACOTE NORMAL                                  *
2011  *             1 - PACOTE CELULAR                                 *
2011  *                                                                *
011209*================================================================*
011209*----------------------------------------------------------------*
011209*      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
011209*----------------------------------------------------------------*
011209*    ULTIMA ALTERACAO EM 01/12/2009 - LAYS                       *
011209*    - AJUSTE NA IDENTIFICACAO DE POSTO COMUM, BANCO POSTAL E    *
011209*    - BRADESCO EXPRESSO.
011209*================================================================*
MAI10 *----------------------------------------------------------------*
MAI10 *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
MAI10 *----------------------------------------------------------------*
MAI10 *    ULTIMA ALTERACAO EM MAI/2010 - FABRICA                      *
MAI10 *    - VERIFICAR SE A TARIFA EH CESTA CELULAR                    *
MAI10 *                                                                *
BI0511*----------------------------------------------------------------*
BI0511*                 U L T I M A   A L T E R A C A O                *
BI0511*----------------------------------------------------------------*
BI0511*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0511       *
BI0511*----------------------------------------------------------------*
BI0511*                                                                *
BI0511*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA/PROCWORK             *
BI0511*    DATA........:  05 / 2011                                    *
BI0511*    OBJETIVO....:  PROJETO CARTAO BONUS CELULAR - FASE II       *
BI0511*                                                                *
BI0412*----------------------------------------------------------------*
BI0412*                 U L T I M A   A L T E R A C A O                *
BI0412*----------------------------------------------------------------*
BI0412*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0412       *
BI0412*----------------------------------------------------------------*
BI0412*                                                                *
BI0412*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI0412*    DATA........:  04 / 2012                                    *
BI0412*    OBJETIVO....:  SETAR FLAG PARA PERMITIR VIZUALIZACAO        *
BI0412*                   DE ADESAO COMPULSORIA, A ADESAO COMPULSORIA  *
BI0412*                   ASSINALADA NO ONLINE SERA INSERIDA NA        *
BI0412*                   BASE GFCTB009 DE ADESAO INDIVIDUAL.          *
BI0412*                                                                *
      *================================================================*
      *                                                                *
      *    ANALISTA....:  CIBELE          - SONDA IT                   *
      *    DATA........:  05 / 2014                                    *
      *    OBJETIVO....:  SUBSTITUIR O ACESSO A BASE YMKCB001          *
      *                   PELO MODULO KCIM0184.                        *
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
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(005) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TABELA             PIC  X(018)         VALUE SPACES.
       77  WRK-COD-TABELA              PIC  X(008)         VALUE SPACES.
       77  WRK-COMANDO-SQL             PIC  X(010)         VALUE SPACES.
       77  WRK-LOCAL-ERRO              PIC  X(004)         VALUE SPACES.
       77  WRK-RESTO                   PIC  9(003)         VALUE ZEROS.
       77  WRK-INTEIRO                 PIC  9(004)         VALUE ZEROS.
       77  WRK-FIM-CURSOR-1            PIC  X(001)         VALUE SPACES.
       77  WRK-TARIFA-VALIDA           PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-AGRUPAMENTO         PIC  X(001)         VALUE SPACES.
       77  WRK-EXISTE-AGRUPAMENTO      PIC  X(001)         VALUE SPACES.
       77  WRK-AGRUPAMENTO-VALIDO      PIC  X(001)         VALUE SPACES.
       77  WRK-PROX-AGRUPAMENTO        PIC  9(003)         VALUE ZEROS.
       77  WRK-SEQ-AGPTO               PIC S9(009) COMP-3  VALUE ZEROS.
       77  WRK-DT-INICIO               PIC  X(010)         VALUE SPACES.
       77  WRK-1-VEZ-SEGMENTO          PIC  X(001)         VALUE 'N'.
       77  WRK-1-VEZ-CNPJ-CPF          PIC  X(001)         VALUE 'N'.
       77  WRK-1-VEZ-UF-MUNIC          PIC  X(001)         VALUE 'N'.
011209 77  WRK-PAB-COMUM               PIC  X(001)         VALUE 'N'.
011209 77  WRK-PAB-POSTAL              PIC  X(001)         VALUE 'N'.
011209 77  WRK-PAB-EXPRESSO            PIC  X(001)         VALUE 'N'.

       01  WRK-DT-PROCM-ATUAL          PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-PROCM-ATUAL-R        REDEFINES WRK-DT-PROCM-ATUAL.
           03  WRK-ANO-PROCM-ATUAL     PIC  9(004).
           03  WRK-MES-PROCM-ATUAL     PIC  9(002).
           03  WRK-DIA-PROCM-ATUAL     PIC  9(002).

       01  WRK-DT-INI-PRMSS-PCOTE      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INI-PRMSS-PCOTE-R    REDEFINES WRK-DT-INI-PRMSS-PCOTE.
           03  WRK-ANO-INI-PRMSS-PCOTE PIC  9(004).
           03  WRK-MES-INI-PRMSS-PCOTE PIC  9(002).
           03  WRK-DIA-INI-PRMSS-PCOTE PIC  9(002).

       01  WRK-DT-INI-ADSAO-INDVD      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INI-ADSAO-INDVD-R    REDEFINES WRK-DT-INI-ADSAO-INDVD.
           03  WRK-AAAAMM-INI-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-INI-ADSAO-INDVD.
               05  WRK-ANO-INI-ADSAO-INDVD
                                       PIC  9(004).
               05  WRK-MES-INI-ADSAO-INDVD
                                       PIC  9(002).
           03  WRK-DIA-INI-ADSAO-INDVD PIC  9(002).

       01  WRK-DT-FIM-ADSAO-INDVD      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-FIM-ADSAO-INDVD-R    REDEFINES WRK-DT-FIM-ADSAO-INDVD.
           03  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-FIM-ADSAO-INDVD.
               05  WRK-ANO-FIM-ADSAO-INDVD
                                       PIC  9(004).
               05  WRK-MES-FIM-ADSAO-INDVD
                                       PIC  9(002).
           03  WRK-DIA-FIM-ADSAO-INDVD PIC  9(002).

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

BI0412 01  WRK-CINDCD-ADSAO-INDVD      PIC  X(001)         VALUE 'I'.
BI0412 01  WRK-CINDCD-ADSAO-AMBAS      PIC  X(001)         VALUE 'A'.

MAI10  01  WRK-CESTA-CELULAR           PIC  X(001)         VALUE SPACES.
MAI10
MAR10 *----------------------------------------------------------------*
MAR10  01  FILLER                      PIC  X(051)         VALUE
MAR10      '* AREA DA POOL0025 *'.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10  01  WRK-POOL0025.
MAR10      05  WRK-P0025-DATA-ENT      PIC  9(008)         VALUE ZEROS.
MAR10      05  WRK-P0025-DIAS          PIC S9(005) COMP-3  VALUE ZEROS.
MAR10      05  WRK-P0025-DATA-SAI      PIC  9(008)         VALUE ZEROS.
MAR10
MAR10  01  WRK-DATA-AMD                PIC  9(008)         VALUE ZEROS.
MAR10  01  FILLER         REDEFINES    WRK-DATA-AMD.
MAR10      05  WRK-ANO-AMD             PIC  9(004).
MAR10      05  WRK-MES-AMD             PIC  9(002).
MAR10      05  WRK-DIA-AMD             PIC  9(002).
MAR10
MAR10  01  WRK-MES-COMP                PIC  9(002) COMP-3  VALUE ZEROS.
MAR10  01  WRK-ANO-COMP                PIC  9(004) COMP-3  VALUE ZEROS.
MAR10
MAR10  01  WRK-DATA-DB2.
MAR10      05  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
MAR10      05  FILLER                  PIC  X(001)         VALUE '.'.
MAR10      05  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
MAR10      05  FILLER                  PIC  X(001)         VALUE '.'.
MAR10      05  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

BI0511 01  WRK-DATA-A2-MAX.
BI0511     05  WRK-DIA-A2-MAX          PIC  9(002)         VALUE ZEROS.
BI0511     05  FILLER                  PIC  X(001)         VALUE '.'.
BI0511     05  WRK-MES-A2-MAX          PIC  9(002)         VALUE ZEROS.
BI0511     05  FILLER                  PIC  X(001)         VALUE '.'.
BI0511     05  WRK-ANO-A2-MAX          PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5527 *'.
      *----------------------------------------------------------------*

       01  WRK-5527-AREA-ENTRADA.
           05  WRK-5527-AMBIENTE           PIC  X(001) VALUE SPACES.
           05  WRK-5527-CHAMADOR           PIC  X(008) VALUE SPACES.
           05  WRK-5527-AGENCIA-CLIENTE    PIC  9(005) VALUE ZEROS.
           05  WRK-5527-CONTA-CLIENTE      PIC  9(013) VALUE ZEROS.

       01  WRK-5527-AREA-SAIDA.
           05  WRK-5527-COD-RETORNO        PIC  9(002) VALUE ZEROS.
           05  WRK-5527-COD-SQL-ERRO       PIC S9(003) VALUE ZEROS.
           05  WRK-5527-COD-MSG-GFCT       PIC  9(004) VALUE ZEROS.
           05  WRK-5527-DESC-MSG           PIC  X(070) VALUE SPACES.
           05  WRK-5527-QTD-OCORR          PIC  9(005) VALUE ZEROS.
           05  WRK-5527-DADOS-RETORNO.
ST25X6*     10  WRK-5527-CCGC-CPF         PIC  9(009) VALUE ZEROS.
ST25X6*     10  WRK-5527-CFLIAL-CGC       PIC  9(004) VALUE ZEROS.
ST25X6*     10  WRK-5527-CCTRL-CPF-CGC    PIC  9(002) VALUE ZEROS.
ST25X6      10  WRK-5527-CCGC-CPF         PIC  X(009) VALUE SPACES.
ST25X6      10  WRK-5527-CFLIAL-CGC       PIC  X(004) VALUE SPACES.
ST25X6      10  WRK-5527-CCTRL-CPF-CGC    PIC  9(002) VALUE ZEROS.
             10  WRK-5527-CPOSTO-SERVC     PIC  9(003) VALUE ZEROS.
             10  WRK-5527-CSGMTO-CLI       PIC  9(003) VALUE ZEROS.

       01  WRK-IO-PCB.
           03  WRK-IO-TERM              PIC  X(008) VALUE SPACES.
           03  FILLER                   PIC  X(002) VALUE SPACES.
           03  WRK-IO-STA               PIC  X(002) VALUE SPACES.
           03  FILLER                   PIC  X(012) VALUE SPACES.
           03  WRK-IO-MODNAME           PIC  X(008) VALUE SPACES.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME          PIC  X(008) VALUE SPACES.
           03  FILLER                   PIC  X(002) VALUE SPACES.
           03  WRK-ALT-STATUS           PIC  X(002) VALUE SPACES.
           03  FILLER                   PIC  X(012) VALUE SPACES.
           03  WRK-ALT-MODNAME          PIC  X(008) VALUE SPACES.

       01  WRK-CPAB-9-05               PIC  9(005)         VALUE ZEROS.
       01  WRK-FILLER                  REDEFINES WRK-CPAB-9-05.
           05 FILLER                   PIC  9(02).
           05 WRK-CPAB-9-03            PIC  9(03).
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INCLUDES *'.
      *----------------------------------------------------------------*

       COPY 'KCIMW184'.

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#GFCTJM'.

       COPY 'I#GFCTRK'.

011209 COPY 'I#MESUM8'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0I0
           END-EXEC.

011209*____EXEC_SQL
011209*______INCLUDE_YMKCB001
011209*____END-EXEC.

MAR10      EXEC SQL
MAR10        INCLUDE GFCTB009
MAR10      END-EXEC.
MAR10
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB0C6 CURSOR FOR
             SELECT DISTINCT(CSERVC_TARIF)
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
BI0412            (CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-INDVD OR
BI0412             CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-AMBAS)    AND
                   DFIM_PRMSS_PCOTE   > :GFCTB0C6.DFIM-PRMSS-PCOTE   AND
                   CSERVC_TARIF       >= :GFCTB0C6.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

MAR10      EXEC SQL
MAR10        DECLARE CSR02-GFCTB009 CURSOR  FOR
MAR10        SELECT
MAR10                CSERVC_TARIF ,
MAR10                HINCL_REG
MAR10        FROM
MAR10                DB2PRD.ADSAO_INDVD_PCOTE
MAR10        WHERE
MAR10                CJUNC_DEPDC       = :GFCTB009.CJUNC-DEPDC
MAR10          AND   CCTA_CLI          = :GFCTB009.CCTA-CLI
MAR10          AND   CINDCD_EXCL_REG   = :GFCTB009.CINDCD-EXCL-REG
MAR10          AND   DFIM_ADSAO_INDVD  > :GFCTB009.DFIM-ADSAO-INDVD
MAR10        ORDER BY
MAR10                HINCL_REG DESC
MAR10      END-EXEC.
MAR10
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTLD'.

       COPY 'I#GFCTLE'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTLD-ENTRADA
                                                      GFCTLE-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTLE-SAIDA.

           INITIALIZE GFCTLE-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSO AO MODULO DE VERIFICACAO DA DISPONIBILIDADE *
      * DO SISTEMA ON-LINE.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE GFCTLD-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTLD-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTLD-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTLE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5124'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE GFCTG1-ERRO        TO GFCTLE-ERRO
               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCTLE-COD-SQL-ERRO
                   MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTLE-COD-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTLE-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5124'     TO GFCT0M-TRANSACAO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTLE-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5124'             TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO E MONTAGEM DA AREA DE SAIDA A PARTIR *
      * DA AREA DE ENTRADA.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE +360                   TO GFCTLE-LL.
           MOVE ZEROS                  TO GFCTLE-ZZ.
           MOVE GFCTLD-TRANSACAO       TO GFCTLE-TRANSACAO.
           MOVE GFCTLD-FUNCAO          TO GFCTLE-FUNCAO.
           MOVE GFCTLD-FILTRO          TO GFCTLE-FILTRO.
           MOVE GFCTLD-PONTEIRO        TO GFCTLE-PONTEIRO.
           MOVE GFCTLD-QTDE-OCOR       TO GFCTLE-QTDE-OCOR.
           MOVE GFCTLD-QTDE-TOT-REG    TO GFCTLE-QTDE-TOT-REG.
           MOVE GFCTLD-FUNC-BDSCO      TO GFCTLE-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTLE-FIM.

           MOVE ZEROS                  TO GFCTLE-ERRO
                                          GFCTLE-COD-SQL-ERRO
                                          GFCTLE-COD-MSG-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTLE-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTLD-TRANSACAO         EQUAL SPACES OR LOW-VALUES) OR
              (GFCTLD-FUNCAO            EQUAL SPACES OR LOW-VALUES) OR
              (GFCTLD-FILTRO-AGENCIA    NOT NUMERIC)                OR
              (GFCTLD-FILTRO-AGENCIA    EQUAL ZEROS)                OR
              (GFCTLD-FILTRO-CONTA      NOT NUMERIC)                OR
              (GFCTLD-FILTRO-CONTA      EQUAL ZEROS)                OR
              (GFCTLD-FILTRO-TIPO-CONTA NOT EQUAL 1 AND 2 AND 3)    OR
              (GFCTLD-FILTRO-TIPO-PESSOA NOT EQUAL 'A' AND 'F'
                                                       AND 'J')     OR
              (GFCTLD-FILTRO-ACESSO     NOT EQUAL 'I' AND 'A')      OR
              (GFCTLD-PONTEIRO-TARIFA   NOT NUMERIC)                OR
              (GFCTLD-QTDE-OCOR         NOT NUMERIC)                OR
              (GFCTLD-QTDE-TOT-REG      NOT NUMERIC)                OR
              (GFCTLD-FUNC-BDSCO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTLD-FIM               NOT EQUAL 'S' AND 'N')
               MOVE 'S'                 TO GFCTLE-FIM
               MOVE 1                   TO GFCTLE-ERRO
               MOVE ZEROS               TO GFCTLE-COD-SQL-ERRO
               MOVE 0001                TO GFCTLE-COD-MSG-ERRO
                                           GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTLE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSO AO MODULO DE DESCRICOES DE MENSAGENS.       *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTLD-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTLD-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTLD-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTLE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5124'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE GFCTG3-ERRO        TO GFCTLE-ERRO
               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTLE-COD-SQL-ERRO
                   MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTLE-COD-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTLE-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5124'     TO GFCT0M-TRANSACAO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 'S'                    TO GFCTLE-FIM
               MOVE GFCTG3-ERRO            TO GFCTLE-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO GFCTLE-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO GFCTLE-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTLE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE PROCESSAMENTO PRINCIPAL DO PROGRAMA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO IND-1.
           MOVE 'N'                    TO WRK-FIM-CURSOR-1.

           PERFORM 2100-OBTER-DATA-PROCESSAMENTO.

MAR10      IF  GFCTLD-FILTRO-ACESSO     EQUAL 'A'
MAR10          PERFORM 2900-TRATAR-GFCTB009
MAR10      END-IF.
MAR10
           PERFORM 2200-ABRIR-CURSOR-1.

           PERFORM 2300-LER-CURSOR-1.

           PERFORM 2400-PROCESSAR-CURSOR-1 UNTIL
                   WRK-FIM-CURSOR-1        EQUAL 'S'.

           PERFORM 2500-FECHAR-CURSOR-1.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER A DATA DE PROCESSAMENTO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-OBTER-DATA-PROCESSAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM ' TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0001'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FORMATACAO PADRAO DE ERROS DB2.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1310-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG ' - '
                  WRK-COD-TABELA
           DELIMITED BY '  '           INTO GFCTLE-DESC-MSG-ERRO.

           MOVE  9                     TO GFCTLE-ERRO.
           MOVE 'GFCT5124'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTLE-COD-SQL-ERRO.
           MOVE WRK-NOME-TABELA        TO GFCT0M-NOME-TAB.
           MOVE WRK-COMANDO-SQL        TO GFCT0M-COMANDO-SQL.
           MOVE WRK-LOCAL-ERRO         TO GFCT0M-LOCAL.

           INITIALIZE GFCTLE-DADOS.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRIR CURSOR 1 DA TABELA GFCTB0C6                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ABRIR-CURSOR-1             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLD-PONTEIRO-TARIFA TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DFIM-PRMSS-PCOTE  OF GFCTB0C6.
BI0412     MOVE 'I'                    TO WRK-CINDCD-ADSAO-INDVD.
BI0412     MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

           EXEC SQL
               OPEN CSR01-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO WRK-NOME-TABELA
               MOVE 'OPEN  '           TO WRK-COMANDO-SQL
               MOVE '0002'             TO WRK-LOCAL-ERRO
               MOVE  0005              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER CURSOR 1 DA TABELA GFCTB0C6                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-CURSOR-1               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0C6 INTO
                   :GFCTB0C6.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO WRK-NOME-TABELA
               MOVE 'FETCH '           TO WRK-COMANDO-SQL
               MOVE '0003'             TO WRK-LOCAL-ERRO
               MOVE  0006              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR-1
                                          GFCTLE-FIM
               IF  IND-1               EQUAL ZEROS
                   PERFORM 2500-FECHAR-CURSOR-1
                   MOVE 1              TO GFCTLE-ERRO
                   MOVE ZEROS          TO GFCTLE-COD-SQL-ERRO
                   MOVE 0846           TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTLE-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR DADOS OBTIDOS NO CURSOR 1 DA TABELA GFCTB0C6.        *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-CURSOR-1         SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-TARIFA-VALIDA.

           PERFORM 2410-OBTER-DADOS-GFCTB0D8.

           IF  CTPO-CTA-DEB-TARIF      OF GFCTB0D8
                                       EQUAL GFCTLD-FILTRO-TIPO-CONTA
                                       AND
               CTPO-SERVC-TARIF OF GFCTB0D8 EQUAL 2
               IF  CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL 3    OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL 1    AND
                   GFCTLD-FILTRO-TIPO-PESSOA       EQUAL 'F') OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL 2    AND
                   GFCTLD-FILTRO-TIPO-PESSOA       EQUAL 'J')
                   PERFORM 2420-CALCULAR-DATA-FIM

                   MOVE 014            TO WRK-PROX-AGRUPAMENTO

                   MOVE 'N'            TO WRK-FIM-AGRUPAMENTO

                   MOVE 'N'            TO WRK-AGRUPAMENTO-VALIDO

                   PERFORM 2430-IDENTIFICAR-PERMISSAO UNTIL
                           WRK-FIM-AGRUPAMENTO        EQUAL 'S' OR
                           WRK-AGRUPAMENTO-VALIDO     EQUAL 'S'

                   IF  WRK-AGRUPAMENTO-VALIDO         EQUAL 'S'
                       MOVE 'S'        TO WRK-TARIFA-VALIDA
                   END-IF
               END-IF
           END-IF.

           IF  WRK-TARIFA-VALIDA       EQUAL 'S'
               PERFORM 2470-MONTAR-OCORRENCIAS
           ELSE
               PERFORM 2300-LER-CURSOR-1
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA TARIFA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-OBTER-DADOS-GFCTB0D8       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD,
                   CPSSOA_SERVC_TARIF,
                   CTPO_CTA_DEB_TARIF,
                   CTPO_SERVC_TARIF
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD,
                   :GFCTB0D8.CPSSOA-SERVC-TARIF,
                   :GFCTB0D8.CTPO-CTA-DEB-TARIF,
                   :GFCTB0D8.CTPO-SERVC-TARIF
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0004'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE FIM DA ADESAO INDIVIDUAL.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-CALCULAR-DATA-FIM          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2421-OBTER-DADOS-GFCTB0A2.

BI0511     MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2
BI0511                                 TO WRK-DATA-A2-MAX.

           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(1:2)
                                       TO WRK-DIA-FIM-ADSAO-INDVD.
           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(4:2)
                                       TO WRK-MES-FIM-ADSAO-INDVD.
           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(7:4)
                                       TO WRK-ANO-FIM-ADSAO-INDVD.

           IF (WRK-MES-FIM-ADSAO-INDVD EQUAL 01 OR 03 OR 05 OR 07 OR
                                             08 OR 10 OR 12) AND
               WRK-DIA-FIM-ADSAO-INDVD EQUAL 31
              NEXT SENTENCE
           ELSE
              IF (WRK-MES-FIM-ADSAO-INDVD EQUAL 04 OR 06 OR
                                                09 OR 11) AND
                  WRK-DIA-FIM-ADSAO-INDVD EQUAL 30
                 NEXT SENTENCE
              ELSE
                 IF WRK-MES-FIM-ADSAO-INDVD EQUAL 02 AND
                    WRK-DIA-FIM-ADSAO-INDVD EQUAL 29
                    NEXT SENTENCE
                 ELSE
                    DIVIDE WRK-ANO-FIM-ADSAO-INDVD BY 4
                                           GIVING WRK-INTEIRO
                                           REMAINDER WRK-RESTO
                    IF WRK-MES-FIM-ADSAO-INDVD EQUAL 02 AND
                       WRK-RESTO               NOT EQUAL ZEROS AND
                       WRK-DIA-FIM-ADSAO-INDVD EQUAL 28
                       NEXT SENTENCE
                    ELSE
                       SUBTRACT 1          FROM WRK-MES-FIM-ADSAO-INDVD

                       EVALUATE WRK-MES-FIM-ADSAO-INDVD

                       WHEN 00
                            MOVE 31        TO WRK-DIA-FIM-ADSAO-INDVD
                            MOVE 12        TO WRK-MES-FIM-ADSAO-INDVD
                            SUBTRACT 1     FROM WRK-ANO-FIM-ADSAO-INDVD

                       WHEN 01
                       WHEN 03
                       WHEN 05
                       WHEN 07
                       WHEN 08
                       WHEN 10
                       WHEN 12
                            MOVE 31        TO WRK-DIA-FIM-ADSAO-INDVD

                       WHEN 04
                       WHEN 06
                       WHEN 09
                       WHEN 11
                            MOVE 30        TO WRK-DIA-FIM-ADSAO-INDVD

                       WHEN 02
                            IF WRK-RESTO   EQUAL ZEROS
                               MOVE 28     TO WRK-DIA-FIM-ADSAO-INDVD
                            ELSE
                               MOVE 29     TO WRK-DIA-FIM-ADSAO-INDVD
                            END-IF
                       END-EVALUATE
                    END-IF
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DO PARAMETRO DA TARIFA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2421-OBTER-DADOS-GFCTB0A2       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6 TO CSERVC-TARIF   OF GFCTB0A2.

           EXEC SQL
             SELECT MAX(DFIM_VGCIA_TARIF)
             INTO
                   :GFCTB0A2.DFIM-VGCIA-TARIF
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0005'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2421-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IDENTIFICAR AGRUPAMENTO COM PERMISSAO DE ADESAO AO PACOTE.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-IDENTIFICAR-PERMISSAO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-EXISTE-AGRUPAMENTO.

           EVALUATE WRK-PROX-AGRUPAMENTO

             WHEN 004
                  IF  WRK-5527-CCGC-CPF               NOT EQUAL ZEROS
                      PERFORM 2432-ACESSAR-GFCTB0I0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 018         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 009
                  IF  WRK-1-VEZ-SEGMENTO  EQUAL  'N'
                      PERFORM 2433-OBTER-SEG-GESTAO
                      MOVE  'S'  TO  WRK-1-VEZ-SEGMENTO
                  END-IF

                  IF  GFCTJM-CSGMTO-GSTAO-TARIF       NOT EQUAL ZEROS
                      PERFORM 2434-ACESSAR-GFCTB0D2

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 'S'         TO WRK-FIM-AGRUPAMENTO
                  END-IF

             WHEN 014
                  PERFORM 2435-ACESSAR-GFCTB0C7

                  IF  WRK-EXISTE-AGRUPAMENTO          EQUAL 'S'
                      PERFORM 2450-OBTER-DADOS-GFCTB0C6
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 015         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 015
                  IF  WRK-1-VEZ-CNPJ-CPF  EQUAL  'N'
                      PERFORM 2431-OBTER-CNPJ-CPF-POSTO
                      MOVE  'S'  TO  WRK-1-VEZ-CNPJ-CPF
                  END-IF

011209            IF  WRK-5527-CPOSTO-SERVC      NOT EQUAL ZEROS AND
011209                WRK-PAB-COMUM              EQUAL 'S'
                      PERFORM 2436-ACESSAR-GFCTB0D0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 017         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 017
011209            IF  WRK-5527-CPOSTO-SERVC      NOT EQUAL ZEROS AND
011209                WRK-PAB-POSTAL             EQUAL 'S'
                      PERFORM 2437-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 020         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 018
                  IF  WRK-1-VEZ-UF-MUNIC  EQUAL  'N'
                      PERFORM 2438-OBTER-UF-MUNICIPIO
                      MOVE  'S'  TO  WRK-1-VEZ-UF-MUNIC
                  END-IF

                  IF  GFCTRK-CMUN-RURAL               NOT EQUAL ZEROS
                      PERFORM 2439-ACESSAR-GFCTB0C9

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 019         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 019
                  IF  GFCTRK-CSGL-UF                  NOT EQUAL SPACES
                      PERFORM 2440-ACESSAR-GFCTB0D3

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 009         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 020
011209            IF  WRK-5527-CPOSTO-SERVC      NOT EQUAL ZEROS AND
011209                WRK-PAB-EXPRESSO           EQUAL 'S'
                      PERFORM 2437-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 2450-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 004         TO WRK-PROX-AGRUPAMENTO
                  END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5527 PARA OBTER CPF/CNPJ/POSTO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       2431-OBTER-CNPJ-CPF-POSTO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO WRK-5527-AREA-SAIDA.

           INITIALIZE WRK-5527-AREA-SAIDA.

           MOVE 'GFCT5124'                 TO WRK-5527-CHAMADOR.
           MOVE GFCTLD-FILTRO-AGENCIA      TO WRK-5527-AGENCIA-CLIENTE.
           MOVE GFCTLD-FILTRO-CONTA        TO WRK-5527-CONTA-CLIENTE.
           MOVE 'O'                        TO WRK-5527-AMBIENTE.

           MOVE 'GFCT5527'                 TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                 WRK-5527-AREA-ENTRADA
                                                 WRK-5527-AREA-SAIDA
                                                 GFCT0M-AREA-ERROS
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO GFCTLE-ERRO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5124'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5527-COD-RETORNO        NOT EQUAL ZEROS AND 02
                                                           AND 05
               IF  WRK-5527-COD-RETORNO    EQUAL 99
                   MOVE 9                  TO GFCTLE-ERRO
               ELSE
                   MOVE 'S'                TO GFCTLE-FIM
                   MOVE 1                  TO GFCTLE-ERRO
               END-IF
               MOVE WRK-5527-COD-MSG-GFCT  TO GFCTLE-COD-SQL-ERRO
               MOVE WRK-5527-COD-SQL-ERRO  TO GFCTLE-COD-MSG-ERRO
               MOVE WRK-5527-DESC-MSG      TO GFCTLE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5527-COD-RETORNO        EQUAL 02 OR 05
               MOVE ZEROS                  TO WRK-5527-CCGC-CPF
                                              WRK-5527-CFLIAL-CGC
                                              WRK-5527-CCTRL-CPF-CGC
           END-IF.

011209     IF  WRK-5527-CPOSTO-SERVC NOT   EQUAL ZEROS
011209         PERFORM 2600-IDENTIFICA-PAB
011209     END-IF.

      *----------------------------------------------------------------*
       2431-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO CLIENTE.*
      ******************************************************************
      *----------------------------------------------------------------*
       2432-ACESSAR-GFCTB0I0           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0I0.
ST25X6*    MOVE WRK-5527-CCGC-CPF      TO CCGC-CPF          OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CCGC-CPF      TO CCGC-CPF-ST       OF GFCTB0I0.
ST25X6*    MOVE WRK-5527-CFLIAL-CGC    TO CFLIAL-CGC        OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CFLIAL-CGC    TO CFLIAL-CGC-ST     OF GFCTB0I0.
ST25X6*    MOVE WRK-5527-CCTRL-CPF-CGC TO CCTRL-CPF-CGC     OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CCTRL-CPF-CGC TO CCTRL-CPF-CGC-ST  OF GFCTB0I0.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0I0.CSEQ-AGPTO-CTA,
                   :GFCTB0I0.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.TPRMSS_GRP_CLI
             WHERE
                   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA        AND
ST25X6*               CCGC_CPF           = :GFCTB0I0.CCGC-CPF          AND
ST25X6*               CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC        AND
ST25X6*               CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC     AND
ST25X6               CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST      AND
ST25X6               CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST    AND
ST25X6               CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.TPRMSS_GRP_CLI
                 WHERE
                   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA        AND
ST25X6*               CCGC_CPF           = :GFCTB0I0.CCGC-CPF          AND
ST25X6*               CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC        AND
ST25X6*               CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC     AND
ST25X6               CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST      AND
ST25X6               CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST    AND
ST25X6               CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0I0.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TPRMSS_GRP_CLI  ' TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0006'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0I0'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0I0 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0I0 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2432-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5534 PARA OBTER SEGMENTO GESTAO.            *
      ******************************************************************
      *----------------------------------------------------------------*
       2433-OBTER-SEG-GESTAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJM-AREA.

           INITIALIZE GFCTJM-AREA.

           MOVE 'O'                    TO GFCTJM-TPO-AMBIENTE.
           MOVE GFCTLD-FILTRO-AGENCIA  TO GFCTJM-CJUNC-DEPDC.
           MOVE GFCTLD-FILTRO-CONTA    TO GFCTJM-CCTA-CLI.

           MOVE 'GFCT5534'                 TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                 GFCTJM-AREA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO GFCTLE-ERRO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5124'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO          EQUAL 99
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5124'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO         NOT EQUAL ZEROS
               MOVE ZEROS                 TO GFCTJM-CSGMTO-GSTAO-TARIF
           END-IF.

      *----------------------------------------------------------------*
       2433-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO SEGMENTO*
      ******************************************************************
      *----------------------------------------------------------------*
       2434-ACESSAR-GFCTB0D2           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D2.
           MOVE GFCTJM-CSGMTO-GSTAO-TARIF
                                       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0D2.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0D2.CSEQ-AGPTO-CTA,
                   :GFCTB0D2.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_SGMTO
             WHERE
                  CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF       AND
                  CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA         AND
                  CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF AND
                  DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_SGMTO
                 WHERE
                  CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF       AND
                  CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA         AND
                  CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF AND
                  DINIC_PRMSS_PCOTE <= :GFCTB0D2.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_GRP_SGMTO ' TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0007'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0D2'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D2 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D2 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2434-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO AGENCIA.*
      ******************************************************************
      *----------------------------------------------------------------*
       2435-ACESSAR-GFCTB0C7           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C7.
           MOVE GFCTLD-FILTRO-AGENCIA  TO CDEPDC            OF GFCTB0C7.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0C7.CSEQ-AGPTO-CTA,
                   :GFCTB0C7.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0C7.CDEPDC)           AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_DEPDC
                 WHERE
                   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0C7.CDEPDC)           AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0C7.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_GRP_DEPDC ' TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0008'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0C7'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0C7 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C7 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2435-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PAB.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2436-ACESSAR-GFCTB0D0           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE GFCTLD-FILTRO-AGENCIA  TO CDEPDC            OF GFCTB0D0.

           MOVE WRK-5527-CPOSTO-SERVC TO CPOSTO-SERVC      OF GFCTB0D0.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0D0.CSEQ-AGPTO-CTA,
                   :GFCTB0D0.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA        AND
                   CDEPDC             = :GFCTB0D0.CDEPDC            AND
                  (CPOSTO_SERVC       = 0                           OR
                   CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_PAB
                 WHERE
                   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA        AND
                   CDEPDC             = :GFCTB0D0.CDEPDC            AND
                  (CPOSTO_SERVC       = 0                           OR
                   CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D0.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_GRP_PAB   ' TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0009'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0D0'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D0 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D0 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2436-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PSTAL.  *
      ******************************************************************
      *----------------------------------------------------------------*
       2437-ACESSAR-GFCTB0D1           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D1.
           MOVE GFCTLD-FILTRO-AGENCIA  TO CDEPDC            OF GFCTB0D1.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D1.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0D1.CSEQ-AGPTO-CTA,
                   :GFCTB0D1.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_PSTAL
             WHERE
                   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0D1.CDEPDC            AND
                   CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_PSTAL
                 WHERE
                   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0D1.CDEPDC            AND
                   CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D1.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_GRP_PSTAL'  TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0010'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0D1'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D1 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D1 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2437-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5564 PARA OBTER UF E MUNICIPIO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       2438-OBTER-UF-MUNICIPIO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA.

           INITIALIZE GFCTRK-AREA.

           MOVE 'O'                    TO GFCTRK-TPO-AMBIENTE.
           MOVE GFCTLD-FILTRO-AGENCIA  TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTLE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5124'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 01 OR 02 OR 99
               MOVE 9                  TO GFCTLE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5124'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       2438-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GP MUNICIPIO. *
      ******************************************************************
      *----------------------------------------------------------------*
       2439-ACESSAR-GFCTB0C9           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
           MOVE GFCTRK-CMUN-RURAL      TO CMUN-IBGE         OF GFCTB0C9.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0C9.CSEQ-AGPTO-CTA,
                   :GFCTB0C9.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_MUN
             WHERE
                   CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA        AND
                   CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE         AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_MUN
                 WHERE
                   CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA        AND
                   CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE         AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0C9.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_GRP_MUN  '  TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0011'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0C9'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0C9 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C9 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2439-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO UF.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-ACESSAR-GFCTB0D3           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D3.
           MOVE GFCTRK-CSGL-UF         TO CSGL-UF           OF GFCTB0D3.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0D3.CSEQ-AGPTO-CTA,
                   :GFCTB0D3.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA        AND
                   CSGL_UF            = :GFCTB0D3.CSGL-UF           AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.PRMSS_GRP_UF
                 WHERE
                   CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA        AND
                   CSGL_UF            = :GFCTB0D3.CSGL-UF           AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D3.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_GRP_UF   '  TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0012'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0D3'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2460-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D3 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D3 TO WRK-DT-INICIO
                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA PERMISSAO DE ADESAO AO PACOTE.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2450-OBTER-DADOS-GFCTB0C6       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE WRK-SEQ-AGPTO          TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
           MOVE WRK-DT-INICIO          TO DINIC-PRMSS-PCOTE OF GFCTB0C6.

BI0412     MOVE 'I'                    TO WRK-CINDCD-ADSAO-INDVD.
BI0412     MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CINDCD_AGPTO_TOT
             INTO
                   :GFCTB0C6.CSEQ-AGPTO-CTA,
                   :GFCTB0C6.DINIC-PRMSS-PCOTE,
                   :GFCTB0C6.CINDCD-AGPTO-TOT
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF        = :GFCTB0C6.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0C6.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0C6.CSEQ-AGPTO-CTA
               AND DINIC_PRMSS_PCOTE   = :GFCTB0C6.DINIC-PRMSS-PCOTE
BI0412         AND (CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-INDVD OR
BI0412              CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-AMBAS)
               AND CIDTFD_PRMSS_INDVD  IN ( 0, 1 )
               AND DFIM_PRMSS_PCOTE    > :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO WRK-COMANDO-SQL
               MOVE '0013'             TO WRK-LOCAL-ERRO
               MOVE  0012              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 'S'                TO WRK-AGRUPAMENTO-VALIDO
           END-IF.

      *----------------------------------------------------------------*
       2450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE INICIO DA ADESAO INDIVIDUAL                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2460-CALCULAR-DATA-INI          SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL OF GFCTB0A1(1:2) TO WRK-DIA-PROCM-ATUAL.
           MOVE DPROCM-ATUAL OF GFCTB0A1(4:2) TO WRK-MES-PROCM-ATUAL.
           MOVE DPROCM-ATUAL OF GFCTB0A1(7:4) TO WRK-ANO-PROCM-ATUAL.
           MOVE DINIC-PRMSS-PCOTE OF GFCTB0C6(1:2)
                                            TO WRK-DIA-INI-PRMSS-PCOTE.
           MOVE DINIC-PRMSS-PCOTE OF GFCTB0C6(4:2)
                                            TO WRK-MES-INI-PRMSS-PCOTE.
           MOVE DINIC-PRMSS-PCOTE OF GFCTB0C6(7:4)
                                            TO WRK-ANO-INI-PRMSS-PCOTE.

           IF  WRK-DT-INI-PRMSS-PCOTE       GREATER WRK-DT-PROCM-ATUAL
               MOVE WRK-DT-INI-PRMSS-PCOTE  TO WRK-DT-INI-ADSAO-INDVD
               IF  WRK-DIA-INI-ADSAO-INDVD  NOT EQUAL 01
                   MOVE 01                  TO WRK-DIA-INI-ADSAO-INDVD
                   ADD 1                    TO WRK-MES-INI-ADSAO-INDVD
                   IF  WRK-MES-INI-ADSAO-INDVD
                                            GREATER 12
                       MOVE 01              TO WRK-MES-INI-ADSAO-INDVD
                       ADD 1                TO WRK-ANO-INI-ADSAO-INDVD
                   END-IF
               END-IF
           ELSE
               MOVE WRK-DT-PROCM-ATUAL      TO WRK-DT-INI-ADSAO-INDVD
               MOVE 01                      TO WRK-DIA-INI-ADSAO-INDVD
               ADD 1                        TO WRK-MES-INI-ADSAO-INDVD
               IF  WRK-MES-INI-ADSAO-INDVD  GREATER 12
                   MOVE 01                  TO WRK-MES-INI-ADSAO-INDVD
                   ADD 1                    TO WRK-ANO-INI-ADSAO-INDVD
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2460-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MOVIMENTAR AS INFORMACOES PARA AREA DE SAIDA.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2470-MONTAR-OCORRENCIAS         SECTION.
      *----------------------------------------------------------------*

MAR10      IF  GFCTLD-FILTRO-ACESSO     EQUAL 'A'
MAR10          IF  CSERVC-TARIF OF GFCTB0C6 EQUAL
MAR10                                      CSERVC-TARIF OF GFCTB009
MAR10              PERFORM 2300-LER-CURSOR-1
MAR10              GO TO 2470-99-FIM
MAR10          END-IF
MAR10      END-IF.
MAR10
           ADD 1                       TO IND-1.

           IF  IND-1                   GREATER 10
               MOVE 'S'                TO WRK-FIM-CURSOR-1
               MOVE CSERVC-TARIF       OF GFCTB0C6
                                       TO GFCTLE-PONTEIRO-TARIFA
               GO TO 2470-99-FIM
           END-IF.

           ADD 65                      TO GFCTLE-LL.
           ADD 1                       TO GFCTLE-QTDE-TOT-REG.
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO GFCTLE-TARIFA-PACOTE(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTLE-DESCR-TARIFA(IND-1).
           MOVE WRK-DIA-INI-ADSAO-INDVD
                                TO GFCTLE-DT-INI-VIG-ADESAO(IND-1)(1:2).
           MOVE WRK-MES-INI-ADSAO-INDVD
                                TO GFCTLE-DT-INI-VIG-ADESAO(IND-1)(4:2).
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                TO GFCTLE-DT-INI-VIG-ADESAO(IND-1)(7:4).
           MOVE '.'             TO GFCTLE-DT-INI-VIG-ADESAO(IND-1)(3:1)
                                   GFCTLE-DT-INI-VIG-ADESAO(IND-1)(6:1).
           MOVE WRK-DIA-FIM-ADSAO-INDVD
                                TO GFCTLE-DT-FIM-VIG-ADESAO(IND-1)(1:2).
           MOVE WRK-MES-FIM-ADSAO-INDVD
                                TO GFCTLE-DT-FIM-VIG-ADESAO(IND-1)(4:2).
           MOVE WRK-ANO-FIM-ADSAO-INDVD
                                TO GFCTLE-DT-FIM-VIG-ADESAO(IND-1)(7:4).
           MOVE '.'             TO GFCTLE-DT-FIM-VIG-ADESAO(IND-1)(3:1)
                                   GFCTLE-DT-FIM-VIG-ADESAO(IND-1)(6:1).

BI0511     PERFORM 2471-ACESSAR-GFCTB0A2
MAI10      IF  WRK-CESTA-CELULAR      NOT EQUAL 'S'
MAI10 *****IF  CSERVC-TARIF OF GFCTB0C6
MAI10 ********************************NOT EQUAL   1123  AND  1124
2011           MOVE 0                 TO GFCTLE-INCD-TARIFA(IND-1)
2011       ELSE
2011           MOVE 1                 TO GFCTLE-INCD-TARIFA(IND-1)
2011       END-IF.

           PERFORM 2300-LER-CURSOR-1.

      *----------------------------------------------------------------*
       2470-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BI0511******************************************************************
BI0511*    ROTINA ACESSAR GFCTB0A2                                     *
BI0511******************************************************************
BI0511*----------------------------------------------------------------*
BI0511 2471-ACESSAR-GFCTB0A2           SECTION.
BI0511*----------------------------------------------------------------*
BI0511
BI0511     MOVE   CSERVC-TARIF OF GFCTB0C6
BI0511                            TO CSERVC-TARIF        OF GFCTB0A2.
BI0511     MOVE   WRK-DATA-A2-MAX
BI0511                            TO DFIM-VGCIA-TARIF    OF GFCTB0A2.
BI0511
BI0511     EXEC SQL
BI0511          SELECT
BI0511               CINDCD_CREDT_CLULR
BI0511          INTO
BI0511              :GFCTB0A2.CINDCD-CREDT-CLULR
BI0511          FROM   DB2PRD.PARM_SERVC_TARIF
BI0511          WHERE  CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
BI0511            AND  DFIM_VGCIA_TARIF   = :GFCTB0A2.DFIM-VGCIA-TARIF
BI0511     END-EXEC.
BI0511
BI0511     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
BI0511        (SQLWARN0                EQUAL 'W'      )
BI0511         MOVE SPACES             TO GFCT0M-ERRO-SQL
BI0511         INITIALIZE GFCT0M-ERRO-SQL
BI0511         MOVE 'PARM_SERVC_TARIF'
BI0511                                 TO WRK-NOME-TABELA
BI0511         MOVE 'SELECT'           TO WRK-COMANDO-SQL
BI0511         MOVE '0019'             TO WRK-LOCAL-ERRO
BI0511         MOVE  0012              TO GFCTLE-COD-MSG-ERRO
BI0511         MOVE '0012'             TO GFCTG2-COD-MSG
BI0511         MOVE 'GFCTB0A2'         TO WRK-COD-TABELA
BI0511         PERFORM 2110-FORMATAR-ERRO-DB2
BI0511     END-IF.
BI0511
BI0511     IF  CINDCD-CREDT-CLULR OF GFCTB0A2   EQUAL   'S'
BI0511         MOVE 'S'                TO WRK-CESTA-CELULAR
BI0511     ELSE
BI0511         MOVE 'N'                TO WRK-CESTA-CELULAR
BI0511     END-IF.
BI0511
BI0511*----------------------------------------------------------------*
BI0511 2471-99-FIM.                    EXIT.
BI0511*----------------------------------------------------------------*

      ******************************************************************
      * FECHAR PRIMEIRO CURSOR DA TABELA GFCTB0C6                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-FECHAR-CURSOR-1            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0C6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PRMSS_ADSAO_PCOTE' TO WRK-NOME-TABELA
               MOVE 'CLOSE '           TO WRK-COMANDO-SQL
               MOVE '0014'             TO WRK-LOCAL-ERRO
               MOVE  0011              TO GFCTLE-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0C6'         TO WRK-COD-TABELA
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

011209*----------------------------------------------------------------*
011209 2600-IDENTIFICA-PAB            SECTION.
011209*----------------------------------------------------------------*
011209
011209     MOVE 'VRS001'                 TO COMU-VERSAO
011209     MOVE 237                      TO COMU-COD-EMPR
011209     MOVE WRK-5527-AGENCIA-CLIENTE TO COMU-COD-DEPDC
011209     MOVE WRK-5527-CPOSTO-SERVC    TO COMU-COD-POSTO
011209     MOVE SPACES                   TO COMU-CORR-BANCARIO
011209
011209     MOVE 'MESU9018'             TO WRK-MODULO
011209
011209     CALL WRK-MODULO              USING COMU-DADOS-9018
011209
011209     IF  RETURN-CODE         NOT EQUAL ZEROS
011209         MOVE 'S'                TO WRK-PAB-COMUM
011209         MOVE 'N'                TO WRK-PAB-POSTAL
011209         MOVE 'N'                TO WRK-PAB-EXPRESSO
011209     ELSE
011209        IF  COMU-COD-TIPO-POSTO NOT EQUAL 9
011209            MOVE 'S'                TO WRK-PAB-COMUM
011209            MOVE 'N'                TO WRK-PAB-POSTAL
011209            MOVE 'N'                TO WRK-PAB-EXPRESSO
011209        ELSE
                  INITIALIZE KCIMW184-AREA-COMUNICACAO
                  MOVE 01                     TO KCIMW184-E-FUNCAO
011209            MOVE WRK-5527-AGENCIA-CLIENTE
011209                                   TO  KCIMW184-E-CAG-BCRIA-1
011209            MOVE WRK-5527-CPOSTO-SERVC
011209                                   TO WRK-CPAB-9-05
011209            MOVE WRK-CPAB-9-03
011209                                   TO KCIMW184-E-CPOSTO-BCRIO-1
011209            PERFORM 2800-LER-YMKCB001
011209            PERFORM 2700-TRATAR-CORRESP
011209        END-IF
011209     END-IF.
011209
011209*----------------------------------------------------------------*
011209 2600-99-FIM.                   EXIT.
011209*----------------------------------------------------------------*
011209
011209*----------------------------------------------------------------*
011209 2700-TRATAR-CORRESP            SECTION.
011209*----------------------------------------------------------------*
011209
           IF  KCIMW184-S-COD-RETORNO EQUAL 98
011209         MOVE 'S'                TO WRK-PAB-COMUM
011209         MOVE 'N'                TO WRK-PAB-POSTAL
011209         MOVE 'N'                TO WRK-PAB-EXPRESSO
011209     ELSE
011209         IF  KCIMW184-S-CEMPR-CORSP (1)
011209                                 NOT EQUAL 10000
011209             MOVE 'N'                TO WRK-PAB-COMUM
011209             MOVE 'N'                TO WRK-PAB-POSTAL
011209             MOVE 'S'                TO WRK-PAB-EXPRESSO
011209         ELSE
011209             MOVE 'N'                TO WRK-PAB-COMUM
011209             MOVE 'S'                TO WRK-PAB-POSTAL
011209             MOVE 'N'                TO WRK-PAB-EXPRESSO
011209         END-IF
011209     END-IF.
011209
011209*----------------------------------------------------------------*
011209 2700-99-FIM.                   EXIT.
011209*----------------------------------------------------------------*
011209
011209*----------------------------------------------------------------*
011209 2800-LER-YMKCB001              SECTION.
011209*----------------------------------------------------------------*
011209
           MOVE 'KCIM0184'             TO    WRK-MODULO.

           CALL WRK-MODULO             USING KCIMW184-AREA-COMUNICACAO.

           IF KCIMW184-S-COD-RETORNO  NOT EQUAL 00 AND 98
              MOVE 1                  TO GFCTLE-ERRO
              MOVE ZEROS              TO GFCTLE-COD-SQL-ERRO
              MOVE 1875               TO GFCTLE-COD-MSG-ERRO
              MOVE '1875'             TO GFCTG2-COD-MSG
              PERFORM 1310-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG    TO GFCTLE-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.
011209
011209*----------------------------------------------------------------*
011209 2800-99-FIM.                   EXIT.
011209*----------------------------------------------------------------*

MAR10 ******************************************************************
MAR10 *    ROTINA TRATAR-GFCTB009                                      *
MAR10 ******************************************************************
MAR10 *----------------------------------------------------------------*
MAR10  2900-TRATAR-GFCTB009            SECTION.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10      PERFORM 2910-ABRIR-CSR02
MAR10
MAR10      PERFORM 2920-LER-CSR02
MAR10
MAR10      PERFORM 2930-FECHAR-CSR02.
MAR10
MAR10 *----------------------------------------------------------------*
MAR10  2900-99-FIM.                    EXIT.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10 ******************************************************************
MAR10 *ABRIR CURSOR DA TABELA GFCTB009                                 *
MAR10 ******************************************************************
MAR10 *----------------------------------------------------------------*
MAR10  2910-ABRIR-CSR02                SECTION.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10      MOVE GFCTLD-FILTRO-AGENCIA  TO CJUNC-DEPDC      OF GFCTB009
MAR10      MOVE GFCTLD-FILTRO-CONTA    TO CCTA-CLI         OF GFCTB009
MAR10      MOVE ZEROS                  TO CINDCD-EXCL-REG  OF GFCTB009
MAR10
MAR10      PERFORM 2911-CALCULAR-DATA
MAR10      MOVE WRK-DATA-DB2           TO DFIM-ADSAO-INDVD OF GFCTB009
MAR10
MAR10      EXEC SQL
MAR10          OPEN CSR02-GFCTB009
MAR10      END-EXEC.
MAR10
MAR10      IF  (SQLCODE                NOT EQUAL ZEROS )OR
MAR10          (SQLWARN0               EQUAL 'W')
MAR10          MOVE SPACES             TO GFCT0M-ERRO-SQL
MAR10          INITIALIZE GFCT0M-ERRO-SQL
MAR10          MOVE 'ADSAO_INDVD_PCOTE' TO WRK-NOME-TABELA
MAR10          MOVE 'OPEN'             TO WRK-COMANDO-SQL
MAR10          MOVE '0016'             TO WRK-LOCAL-ERRO
MAR10          MOVE  0011              TO GFCTLE-COD-MSG-ERRO
MAR10          MOVE '0011'             TO GFCTG2-COD-MSG
MAR10          MOVE 'GFCTB009'         TO WRK-COD-TABELA
MAR10          PERFORM 2110-FORMATAR-ERRO-DB2
MAR10      END-IF.
MAR10
MAR10 *----------------------------------------------------------------*
MAR10  2910-99-FIM.                    EXIT.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10 ******************************************************************
MAR10 *    ROTINA CALCULAR-DATA: ULTIMO DIA DO MES CORRENTE
MAR10 ******************************************************************
MAR10 *----------------------------------------------------------------*
MAR10  2911-CALCULAR-DATA              SECTION.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10      MOVE DPROCM-ATUAL OF GFCTB0A1
MAR10                                  TO WRK-DATA-DB2
MAR10      MOVE WRK-MES-DB2            TO WRK-MES-COMP
MAR10      MOVE WRK-ANO-DB2            TO WRK-ANO-COMP
MAR10
MAR10      IF  WRK-MES-COMP EQUAL 12
MAR10          SUBTRACT 12             FROM WRK-MES-COMP
MAR10          ADD 1                   TO WRK-ANO-COMP
MAR10      END-IF.
MAR10
MAR10      ADD 1                       TO WRK-MES-COMP
MAR10      MOVE 1                      TO WRK-DIA-AMD
MAR10      MOVE WRK-MES-COMP           TO WRK-MES-AMD
MAR10      MOVE WRK-ANO-COMP           TO WRK-ANO-AMD
MAR10
MAR10      MOVE WRK-DATA-AMD           TO WRK-P0025-DATA-ENT
MAR10      MOVE -1                     TO WRK-P0025-DIAS
MAR10      MOVE ZEROS                  TO WRK-P0025-DATA-SAI
MAR10
MAR10      CALL 'POOL0025'          USING WRK-P0025-DATA-ENT
MAR10                                     WRK-P0025-DIAS
MAR10                                     WRK-P0025-DATA-SAI
MAR10
MAR10      MOVE WRK-P0025-DATA-SAI     TO WRK-DATA-AMD
MAR10      MOVE WRK-DIA-AMD            TO WRK-DIA-DB2
MAR10      MOVE WRK-MES-AMD            TO WRK-MES-DB2
MAR10      MOVE WRK-ANO-AMD            TO WRK-ANO-DB2.
MAR10
MAR10 *----------------------------------------------------------------*
MAR10  2911-99-FIM.                    EXIT.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10 ******************************************************************
MAR10 * LER CURSOR DA TABELA GFCTB009                                  *
MAR10 ******************************************************************
MAR10 *----------------------------------------------------------------*
MAR10  2920-LER-CSR02                  SECTION.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10      EXEC SQL
MAR10        FETCH CSR02-GFCTB009   INTO
MAR10              :GFCTB009.CSERVC-TARIF ,
MAR10              :GFCTB009.HINCL-REG
MAR10      END-EXEC.
MAR10
MAR10      IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
MAR10          (SQLWARN0               EQUAL 'W')
MAR10          MOVE SPACES             TO GFCT0M-ERRO-SQL
MAR10          INITIALIZE GFCT0M-ERRO-SQL
MAR10          MOVE 'ADSAO_INDVD_PCOTE' TO WRK-NOME-TABELA
MAR10          MOVE 'FETCH'            TO WRK-COMANDO-SQL
MAR10          MOVE '0017'             TO WRK-LOCAL-ERRO
MAR10          MOVE  0011              TO GFCTLE-COD-MSG-ERRO
MAR10          MOVE '0011'             TO GFCTG2-COD-MSG
MAR10          MOVE 'GFCTB009'         TO WRK-COD-TABELA
MAR10          PERFORM 2110-FORMATAR-ERRO-DB2
MAR10      END-IF.
MAR10
MAR10      IF  SQLCODE                 EQUAL +100
MAR10          MOVE 1                  TO GFCTLE-ERRO
MAR10          MOVE ZEROS              TO GFCTLE-COD-SQL-ERRO
MAR10          MOVE 1672               TO GFCTLE-COD-MSG-ERRO
MAR10          MOVE '1672'             TO GFCTG2-COD-MSG
MAR10          PERFORM 1310-OBTER-DESC-MSG
MAR10          MOVE GFCTG3-DESC-MSG TO GFCTLE-DESC-MSG-ERRO
MAR10          PERFORM 3000-FINALIZAR
MAR10      END-IF.
MAR10
MAR10 *----------------------------------------------------------------*
MAR10  2920-99-FIM.                    EXIT.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10 ******************************************************************
MAR10 *    FECHAR CURSOR DA TABELA GFCTB009                            *
MAR10 ******************************************************************
MAR10 *----------------------------------------------------------------*
MAR10  2930-FECHAR-CSR02               SECTION.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10      EXEC SQL
MAR10          CLOSE CSR02-GFCTB009
MAR10      END-EXEC.
MAR10
MAR10      IF  (SQLCODE                NOT EQUAL ZEROS )OR
MAR10          (SQLWARN0               EQUAL 'W')
MAR10          MOVE SPACES             TO GFCT0M-ERRO-SQL
MAR10          INITIALIZE GFCT0M-ERRO-SQL
MAR10          MOVE 'ADSAO_INDVD_PCOTE' TO WRK-NOME-TABELA
MAR10          MOVE 'CLOSE'            TO WRK-COMANDO-SQL
MAR10          MOVE '0018'             TO WRK-LOCAL-ERRO
MAR10          MOVE  0011              TO GFCTLE-COD-MSG-ERRO
MAR10          MOVE '0011'             TO GFCTG2-COD-MSG
MAR10          MOVE 'GFCTB009'         TO WRK-COD-TABELA
MAR10          PERFORM 2110-FORMATAR-ERRO-DB2
MAR10      END-IF.
MAR10
MAR10 *----------------------------------------------------------------*
MAR10  2930-99-FIM.                    EXIT.
MAR10 *----------------------------------------------------------------*
MAR10
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
