      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT0574.
       AUTHOR.     PAGNOCCA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0574                                    *
      *    PROGRAMADOR.:   PAGNOCCA                                    *
      *    DATA........:   20/12/2010                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   MODULO PARA ADESAO A CESTA DE SERVICOS      *
      *                    CONTAS ABERTAS NA PLATAFORMA DE NEGOCIOS.   *
      *                    ESPECIFICO PARA ADESAO CESTA CELULAR.       *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                     INCLUDE/BOOK        *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *                   DB2PRD.PARM_SERVC_TARIF    GFCTB0A2          *
      *                   DB2PRD.PRMSS_ADSAO_PCOTE   GFCTB0C6          *
      *                   DB2PRD.PRMSS_GRP_DEPDC     GFCTB0C7          *
      *                   DB2PRD.PRMSS_GRP_MUN       GFCTB0C9          *
      *                   DB2PRD.PRMSS_GRP_PAB       GFCTB0D0          *
      *                   DB2PRD.PRMSS_GRP_PSTAL     GFCTB0D1          *
      *                   DB2PRD.PRMSS_GRP_SGMTO     GFCTB0D2          *
      *                   DB2PRD.PRMSS_GRP_UF        GFCTB0D3          *
      *                   DB2PRD.SERVC_TARIF_PRINC   GFCTB0D8          *
      *                   DB2PRD.TPRMSS_GRP_CLI      GFCTB0I0          *
      *                   DB2PRD.V01CLIENTE_AGENCIA  CLIEV004          *
      *                   DB2PRD.V01CTA_POUPANCA     CLIEV007          *
      *                   DB2PRD.V01CTA_CORRENTE     CLIEV008          *
      *                   DB2PRD.ADSAO_INDVD_PCOTE   GFCTB009          *
      *                   DB2PRD.TADSAO_INDVD_CLULR  GFCTB0M2          *
      *                   DB2PRD.TLOG_SOLTC_CORSP    GFCTB0L9          *
      *                   DB2PRD.TOPER_PRMSS_CLULR   GFCTB0N7          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWAI2 - ENTRADA - MODULO CORPORATIVO GFCT0574            *
      *    GFCTWAI3 - SAIDA   - MODULO CORPORATIVO GFCT0574            *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTJM - ENTRADA E SAIDA - OBTER COD SEG GESTAO TARIFA.   *
      *    I#GFCTRK - ENTRADA E SAIDA - OBTER COD UF E MUNICIPIO DA    *
      *               AGENCIA.                                         *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5527 - OBTER CPF/CNPJ/POSTO DE SERVICO.                 *
      *    GFCT5534 - OBTER CODIGO SEGMENTO GESTAO TARIFA.             *
      *    GFCT5564 - OBTER COD DA UF E MUNICIPIO DA AGENCIA.          *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *================================================================*
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   FABRICA SOFTWARE            - PROCWORK      *
      *    ANALISTA....:   MARCUS VINICIUS             - PROCWORK      *
      *    DATA........:   ABR/2010                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   BLOQUEAR ENTRADA CESTA CELULAR.             *
      *================================================================*
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
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-TIPO-PESSOA-CLIENTE     PIC  X(001)         VALUE SPACES.
       77  WRK-FLAG-OK                 PIC  X(001)         VALUE SPACES.
       77  WRK-TIPO-CONTA-CLIENTE      PIC S9(001) COMP-3  VALUE ZEROS.
       77  WRK-RESTO                   PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-INTEIRO                 PIC  9(004) COMP-3  VALUE ZEROS.
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
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE 'N'.
       77  WRK-EXISTE-COMPONENTE       PIC  X(001)         VALUE 'N'.
       77  WRK-COD-MSG                 PIC  9(004)         VALUE ZEROS.
       77  WRK-TIMESTAMP               PIC  X(026)         VALUE SPACES.
       77  WRK-COD-RETORNO             PIC  9(003)         VALUE ZEROS.

       77  WRK-INI-ANO                 PIC  9(004) COMP-3  VALUE ZEROS.
       77  WRK-INI-MES                 PIC  9(002) COMP-3  VALUE ZEROS.

       01  WRK-NUM-CEL                 PIC +9(11)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-NUM-CEL.
           05 WRK-NUM-CEL-S            PIC  X(01).
           05 WRK-NUM-CEL-Z            PIC  9(02).
           05 WRK-NUM-CEL-9            PIC  9(01).
           05 WRK-NUM-CEL-8            PIC  9(08).
       01  FILLER            REDEFINES WRK-NUM-CEL.
           05 WRK-NUM-CEL-S2           PIC  X(01).
           05 WRK-NUM-CEL-11           PIC  9(11).

       01  WRK-TAR-CEL                 PIC +9(05)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-TAR-CEL.
           05 WRK-TAR-CEL-S            PIC  X(01).
           05 WRK-TAR-CEL-5            PIC  9(05).

       01  WRK-OPE-CEL                 PIC +9(09)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-OPE-CEL.
           05 WRK-OPE-CEL-S            PIC  X(01).
           05 WRK-OPE-CEL-9            PIC  9(09).

       01  WRK-DDD-CEL                 PIC +9(03)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-DDD-CEL.
           05 WRK-DDD-CEL-S            PIC  X(01).
           05 WRK-DDD-CEL-3            PIC  9(03).


       01  WRK-CMUN.
           05 FILLER                   PIC X(002)          VALUE SPACE.
           05 WRK-CMUN-7               PIC 9(007)          VALUE ZEROS.
       01  WRK-CMUN-R REDEFINES WRK-CMUN
                                       PIC 9(009).
       01  WRK-WLOG-REG-OCOR.
           03  WRK-VERSAO              PIC  9(003)         VALUE ZEROS.
           03  WRK-PAB                 PIC  9(005)         VALUE ZEROS.
           03  WRK-SEGMENTO            PIC  9(003)         VALUE ZEROS.
           03  WRK-DT-CONTA            PIC  9(008)         VALUE ZEROS.
           03  FILLER                  PIC  X(050)         VALUE SPACES.

       01  WRK-DT-PROC-A1              PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-PROC-A1-R            REDEFINES WRK-DT-PROC-A1.
           03  WRK-ANO-PROC-A1         PIC  X(004).
           03  WRK-MES-PROC-A1         PIC  X(002).
           03  WRK-DIA-PROC-A1         PIC  X(002).

       01  WRK-DT-PROCM-ATUAL          PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-PROCM-ATUAL-R        REDEFINES WRK-DT-PROCM-ATUAL.
           03  WRK-ANO-PROCM-ATUAL     PIC  X(004).
           03  WRK-MES-PROCM-ATUAL     PIC  X(002).
           03  WRK-DIA-PROCM-ATUAL     PIC  X(002).

       01  WRK-DT-PROCM                PIC  9(006)         VALUE ZEROS.
       01  WRK-DT-PROCM-R              REDEFINES WRK-DT-PROCM.
           03  WRK-MES-PROCM           PIC  X(002).
           03  WRK-ANO-PROCM           PIC  X(004).

       01  WRK-DT-ABERT                PIC  9(006)         VALUE ZEROS.
       01  WRK-DT-ABERT-R              REDEFINES WRK-DT-ABERT.
           03  WRK-MES-ABERT           PIC  9(002).
           03  WRK-ANO-ABERT           PIC  9(004).

       01  WRK-DT-INI-PRMSS-PCOTE      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INI-PRMSS-PCOTE-R    REDEFINES WRK-DT-INI-PRMSS-PCOTE.
           03  WRK-ANO-INI-PRMSS-PCOTE PIC  X(004).
           03  WRK-MES-INI-PRMSS-PCOTE PIC  X(002).
           03  WRK-DIA-INI-PRMSS-PCOTE PIC  X(002).

       01  WRK-DT-INI-ADSAO-INDVD      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INI-ADSAO-INDVD-R    REDEFINES WRK-DT-INI-ADSAO-INDVD.
           03  WRK-AAAAMM-INI-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-INI-ADSAO-INDVD.
               05  WRK-ANO-INI-ADSAO-INDVD
                                       PIC  9(004).
               05  WRK-ANO-INI-ADSAO-INDVD-X REDEFINES
                   WRK-ANO-INI-ADSAO-INDVD PIC  X(004).

               05  WRK-MES-INI-ADSAO-INDVD PIC  9(002).

               05  WRK-MES-INI-ADSAO-INDVD-X REDEFINES
                   WRK-MES-INI-ADSAO-INDVD PIC  X(002).

           03  WRK-DIA-INI-ADSAO-INDVD PIC  9(002).
           03  WRK-DIA-INI-ADSAO-INDVD-X REDEFINES
               WRK-DIA-INI-ADSAO-INDVD PIC  X(002).

       01  WRK-DT-FIM-ADSAO-INDVD      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-FIM-ADSAO-INDVD-R    REDEFINES WRK-DT-FIM-ADSAO-INDVD.
           03  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-FIM-ADSAO-INDVD.
               05  WRK-ANO-FIM-ADSAO-INDVD
                                       PIC  9(004).
               05  WRK-ANO-FIM-ADSAO-INDVD-X REDEFINES
                                                 WRK-ANO-FIM-ADSAO-INDVD
                                       PIC  X(004).
               05  WRK-MES-FIM-ADSAO-INDVD
                                       PIC  9(002).
               05  WRK-MES-FIM-ADSAO-INDVD-X REDEFINES
                                                 WRK-MES-FIM-ADSAO-INDVD
                                       PIC  X(002).
           03  WRK-DIA-FIM-ADSAO-INDVD PIC  9(002).
           03  WRK-DIA-FIM-ADSAO-INDVD-X REDEFINES
                                    WRK-DIA-FIM-ADSAO-INDVD PIC  X(002).

       01  WRK-CESTA-CELULAR           PIC  X(001)         VALUE SPACES.

BI0412 01  WRK-CINDCD-ADSAO-INDVD      PIC  X(001)         VALUE 'I'.
BI0412 01  WRK-CINDCD-ADSAO-AMBAS      PIC  X(001)         VALUE 'A'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5527 *'.
      *----------------------------------------------------------------*

       01  WRK-5527-AREA-ENTRADA.
           05  WRK-5527-AMBIENTE           PIC  X(001)     VALUE SPACES.
           05  WRK-5527-CHAMADOR           PIC  X(008)     VALUE SPACES.
           05  WRK-5527-AGENCIA-CLIENTE    PIC  9(005)     VALUE ZEROS.
           05  WRK-5527-CONTA-CLIENTE      PIC  9(013)     VALUE ZEROS.

       01  WRK-5527-AREA-SAIDA.
           05  WRK-5527-COD-RETORNO        PIC  9(002)     VALUE ZEROS.
           05  WRK-5527-COD-SQL-ERRO       PIC S9(003)     VALUE ZEROS.
           05  WRK-5527-COD-MSG-GFCT       PIC  9(004)     VALUE ZEROS.
           05  WRK-5527-DESC-MSG           PIC  X(070)     VALUE SPACES.
           05  WRK-5527-QTD-OCORR          PIC  9(005)     VALUE ZEROS.
           05  WRK-5527-DADOS-RETORNO.
             10  WRK-5527-CCGC-CPF         PIC  9(009)     VALUE ZEROS.
             10  WRK-5527-CFLIAL-CGC       PIC  9(004)     VALUE ZEROS.
             10  WRK-5527-CCTRL-CPF-CGC    PIC  9(002)     VALUE ZEROS.
             10  WRK-5527-CPOSTO-SERVC     PIC  9(003)     VALUE ZEROS.
             10  WRK-5527-CSGMTO-CLI       PIC  9(003)     VALUE ZEROS.

       01  WRK-IO-PCB.
           03  WRK-IO-TERM                 PIC  X(008)     VALUE SPACES.
           03  FILLER                      PIC  X(002)     VALUE SPACES.
           03  WRK-IO-STA                  PIC  X(002)     VALUE SPACES.
           03  FILLER                      PIC  X(012)     VALUE SPACES.
           03  WRK-IO-MODNAME              PIC  X(008)     VALUE SPACES.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME             PIC  X(008)     VALUE SPACES.
           03  FILLER                      PIC  X(002)     VALUE SPACES.
           03  WRK-ALT-STATUS              PIC  X(002)     VALUE SPACES.
           03  FILLER                      PIC  X(012)     VALUE SPACES.
           03  WRK-ALT-MODNAME             PIC  X(008)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA MENSAGENS MODULO GFCT5523 - MENSAGENS *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-1                    PIC  X(070) VALUE
           'ADESAO AO PACOTE - NAO FOI POSSIVEL OBTER A MSG NO MODULO
      -    'GFCT5523'.

       01  WRK-MSG-2                    PIC  X(070) VALUE
           'ADESAO AO PACOTE - GFCTG3-ERRO IGUAL A 9'.

       01  WRK-MSG-3                    PIC  X(070) VALUE
           'ADESAO AO PACOTE - GFCTG3-ERRO IGUAL A 1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INCLUDES *'.
      *----------------------------------------------------------------*
       COPY 'GFCTWAN7'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#GFCTJM'.

       COPY 'I#GFCTRK'.

       COPY 'I#SCCE91'.

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

           EXEC SQL
             INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB009
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0M4
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB029
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0L9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0M2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0N7
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSOR *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB029 CURSOR FOR
             SELECT DISTINCT(CTARIF_COMPO_PCOTE)
             FROM  DB2PRD.COMPS_TARIF_PCOTE
             WHERE
                   CTARIF_PCOTE       = :GFCTB029.CTARIF-PCOTE
             ORDER BY CTARIF_COMPO_PCOTE
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWAI2'.

       COPY 'GFCTWAI3'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTWAI2-ENTRADA
                                                      GFCTWAI3-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR-REGISTROS.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO SQLCA
                                          GFCTWAI3-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                     SQLCA
                                          GFCTWAI3-SAIDA
                                          GFCT0M-AREA-ERROS.

           MOVE +145                   TO GFCTWAI3-LL.
           MOVE GFCTWAI2-ZZ            TO GFCTWAI3-ZZ.
           MOVE GFCTWAI2-TRANSACAO     TO GFCTWAI3-TRANSACAO.
           MOVE GFCTWAI2-FUNCAO        TO GFCTWAI3-FUNCAO.
           MOVE '0000'                 TO GFCTG2-COD-MSG
           MOVE 0000                   TO GFCTWAI3-COD-MSG-RETORNO
           PERFORM 1500-OBTER-DESC-MSG

           MOVE GFCTG3-DESC-MSG        TO GFCTWAI3-COD-MSG-GFCT.

           PERFORM 1100-CONSISTIR-DADOS.
           PERFORM 1300-OBTER-DATA-PROCESSAMENTO.
           PERFORM 1400-CONSISTE-PACOTE.
           PERFORM 1150-VALIDAR-CONTA.
           PERFORM 1200-CONSISTE-AG-CTA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTWAI2-AGENCIA           NOT NUMERIC)                OR
              (GFCTWAI2-AGENCIA           EQUAL ZEROS)                OR
              (GFCTWAI2-CONTA             NOT NUMERIC)                OR
              (GFCTWAI2-CONTA             EQUAL ZEROS)                OR
              (GFCTWAI2-PAB               NOT NUMERIC)                OR
              (GFCTWAI2-PACOTE            NOT NUMERIC)                OR
              (GFCTWAI2-SEGMENTO-CLIE     NOT NUMERIC)                OR
              (GFCTWAI2-DT-ABERTURA-CONTA NOT NUMERIC)                OR
              (GFCTWAI2-VERSAO            NOT NUMERIC)                OR
              (GFCTWAI2-VERSAO            NOT =  002 )                OR
              (GFCTWAI2-DDD               NOT NUMERIC)                OR
              (GFCTWAI2-DDD-N             EQUAL ZEROS)                OR
              (GFCTWAI2-NUMERO            NOT NUMERIC)                OR
              (GFCTWAI2-NUMERO-N          EQUAL ZEROS)                OR
              (GFCTWAI2-OPERADORA         NOT NUMERIC)                OR
              (GFCTWAI2-OPERADORA-N       EQUAL ZEROS)
               MOVE '1597'                TO GFCTG2-COD-MSG
               MOVE 1597                  TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                     TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG       TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                    TO RETURN-CODE
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-VALIDAR-CONTA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAI2-AGENCIA       TO LKW-CJUNC-DEPC.
           MOVE GFCTWAI2-CONTA         TO LKW-CTA-CORR.

           MOVE 'SCCE9100'             TO WRK-MODULO.

           CALL WRK-MODULO             USING
                                             LKW-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE '1604'             TO GFCTG2-COD-MSG
               MOVE 1604               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1604               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  LKW-CD-RETORNO          EQUAL 99
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE '1604'             TO GFCTG2-COD-MSG
               MOVE 1604               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1604               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  LKW-CD-RETORNO          NOT EQUAL ZEROS
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE '1605'             TO GFCTG2-COD-MSG
               MOVE 1605               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1605               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-CONSISTE-AG-CTA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAI2-AGENCIA  TO CJUNC-DEPDC      OF CLIEV007.
           MOVE GFCTWAI2-CONTA    TO CCTA-CLI         OF CLIEV007.

           EXEC SQL
             SELECT
                   CID_CLI
             INTO
                   :CLIEV007.CID-CLI
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE
                   CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC AND
                   CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1314'             TO GFCTG2-COD-MSG
               MOVE 1314               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1314               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 1210-ACESSAR-CLIEV008
           ELSE
               MOVE '1620'             TO GFCTG2-COD-MSG
               MOVE 1620               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1620               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV008                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-ACESSAR-CLIEV008           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAI2-AGENCIA  TO CJUNC-DEPDC      OF CLIEV008.
           MOVE GFCTWAI2-CONTA    TO CCTA-CLI         OF CLIEV008.

           EXEC SQL
             SELECT
                   CID_CLI
             INTO
                   :CLIEV008.CID-CLI
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                   CJUNC_DEPDC         = :CLIEV008.CJUNC-DEPDC AND
                   CCTA_CLI            = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1315'             TO GFCTG2-COD-MSG
               MOVE 1315               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1315               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE '1322'             TO GFCTG2-COD-MSG
               MOVE 1322               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1322               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           ELSE
               MOVE 1                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV008
                                       TO CID-CLI          OF CLIEV004
               PERFORM 1220-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV004 PARA OBTER DADOS DO CLIENTE                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
                   CTPO_PSSOA,
                   IPSSOA_COPLT
             INTO
                   :CLIEV004.CCGC-CPF,
                   :CLIEV004.CFLIAL-CGC,
                   :CLIEV004.CCTRL-CPF-CGC,
                   :CLIEV004.CTPO-PSSOA,
                   :CLIEV004.IPSSOA-COPLT
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE
                   CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1316'             TO GFCTG2-COD-MSG
               MOVE 1316               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1316               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE '1323'             TO GFCTG2-COD-MSG
               MOVE 1323               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1323               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           ELSE
               IF  CFLIAL-CGC OF CLIEV004
                                       EQUAL ZEROS
                   MOVE 'F'            TO WRK-TIPO-PESSOA-CLIENTE
               ELSE
                   MOVE 'J'            TO WRK-TIPO-PESSOA-CLIENTE
               END-IF
               PERFORM 3010-OBTER-DADOS-GFCTB0D8
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER A DATA DE PROCESSAMENTO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OBTER-DATA-PROCESSAMENTO   SECTION.
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
               MOVE '1309'             TO GFCTG2-COD-MSG
               MOVE 1309               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1309               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           MOVE GFCTWAI2-NUMERO-N      TO WRK-NUM-CEL-8
           MOVE '+'                    TO WRK-NUM-CEL-S
           MOVE ZEROS                  TO WRK-NUM-CEL-Z
                                          WRK-NUM-CEL-9.

           MOVE DPROCM-ATUAL OF GFCTB0A1(1:2) TO WRK-DIA-PROC-A1.
           MOVE DPROCM-ATUAL OF GFCTB0A1(4:2) TO WRK-MES-PROC-A1.
           MOVE DPROCM-ATUAL OF GFCTB0A1(7:4) TO WRK-ANO-PROC-A1.


           IF WRK-DT-PROC-A1 NOT LESS 20120729
              IF GFCTWAI2-DDD-N = 011
                 MOVE 9         TO WRK-NUM-CEL-9
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-CONSISTE-PACOTE           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAI2-PACOTE TO CSERVC-TARIF       OF GFCTB0C6.
           MOVE DPROCM-ATUAL    TO DFIM-PRMSS-PCOTE   OF GFCTB0C6.

BI0412     MOVE 'I'             TO WRK-CINDCD-ADSAO-INDVD.
BI0412     MOVE 'A'             TO WRK-CINDCD-ADSAO-AMBAS.

           EXEC SQL
             SELECT
                   CSERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
               WHERE
                   CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF     AND
                   DFIM_PRMSS_PCOTE   > :GFCTB0C6.DFIM-PRMSS-PCOTE AND
BI0412            (CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-INDVD OR
BI0412             CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-AMBAS)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1339'             TO GFCTG2-COD-MSG
               MOVE 1339               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1339               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE '1340'             TO GFCTG2-COD-MSG
               MOVE 1340               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1340               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.


      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSO AO MODULO DE DESCRICOES DE MENSAGENS.       *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTWAI2-TRANSACAO     TO GFCTG2-TRANSACAO.
           MOVE GFCTWAI2-FUNCAO        TO GFCTG2-FUNCAO.
           MOVE '0572'                 TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE 1305               TO GFCTWAI3-COD-MSG-RETORNO
               MOVE WRK-MSG-1          TO GFCTWAI3-TXT-MSG-RETORNO
               MOVE 12                 TO RETURN-CODE
               MOVE 1305               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE 1306               TO GFCTWAI3-COD-MSG-RETORNO
               MOVE WRK-MSG-2          TO GFCTWAI3-TXT-MSG-RETORNO
               MOVE 12                 TO RETURN-CODE
               MOVE 1306               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE 1307               TO GFCTWAI3-COD-MSG-RETORNO
               MOVE WRK-MSG-3          TO GFCTWAI3-TXT-MSG-RETORNO
               MOVE 12                 TO RETURN-CODE
               MOVE 1307               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      * PROCESSAR DADOS OBTIDOS NO CURSOR 1 DA TABELA GFCTB0C6.        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR-REGISTROS         SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-TARIFA-VALIDA.

           IF  CTPO-CTA-DEB-TARIF      OF GFCTB0D8
                                       EQUAL WRK-TIPO-CONTA-CLIENTE
               IF  CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '3'    OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '1'    AND
                   WRK-TIPO-PESSOA-CLIENTE       EQUAL 'F')   OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '2'    AND
                   WRK-TIPO-PESSOA-CLIENTE       EQUAL 'J')
                   PERFORM 2200-CALCULAR-DATA-FIM

                   MOVE 014            TO WRK-PROX-AGRUPAMENTO

                   MOVE 'N'            TO WRK-FIM-AGRUPAMENTO

                   MOVE 'N'            TO WRK-AGRUPAMENTO-VALIDO

                   PERFORM 3020-IDENTIFICAR-PERMISSAO UNTIL
                           WRK-FIM-AGRUPAMENTO        EQUAL 'S' OR
                           WRK-AGRUPAMENTO-VALIDO     EQUAL 'S'

                   IF  WRK-AGRUPAMENTO-VALIDO         EQUAL 'S'
BI0511                 PERFORM 2300-ACESSAR-GFCTB0A2
                       IF  WRK-CESTA-CELULAR          EQUAL 'N'
                           MOVE '1795'     TO GFCTG2-COD-MSG
                           MOVE 1795       TO GFCTWAI3-COD-MSG-RETORNO
                           PERFORM 1500-OBTER-DESC-MSG
                           MOVE 1          TO GFCTWAI3-COD-RETORNO
                                           WRK-COD-RETORNO
                           MOVE GFCTG3-DESC-MSG
                                           TO GFCTWAI3-COD-MSG-GFCT
                           MOVE 12         TO RETURN-CODE
                           MOVE 1795       TO WRK-COD-MSG
                           PERFORM 3300-INSERIR-GFCTB0L9
                           MOVE WRK-TIMESTAMP
                                           TO GFCTWAI3-LOG-TIMESTAMP
                           MOVE GFCTWAI2-TRANSACAO(1:8)
                                           TO GFCTWAI3-LOG-TRANSACAO
                           MOVE GFCTWAI2-FUNCAO
                                           TO GFCTWAI3-LOG-FUNCAO
                           PERFORM 4000-FINALIZAR
                       ELSE
                           MOVE 'S'        TO WRK-TARIFA-VALIDA
                       END-IF
                   END-IF
               END-IF
           END-IF.

           IF  WRK-TARIFA-VALIDA       EQUAL 'S'
               PERFORM 3150-VER-TARIFA-PACOTE-CAD
               PERFORM 3500-VERIFICA-CELULAR
               PERFORM 3700-VERIFICA-OPERADORA
               PERFORM 3100-VER-TARIFA-COMP-CAD
               IF WRK-DT-PROC-A1 NOT LESS 20120729
                  PERFORM 3105-VERIFICA-PERMISSAO-ADESAO
               END-IF
               PERFORM 3200-INSERIR-GFCTB009
               PERFORM 3250-INSERIR-GFCTB0M2
           ELSE
               MOVE '1606'             TO GFCTG2-COD-MSG
               MOVE 1606               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1606               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE INICIO DA ADESAO INDIVIDUAL                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CALCULAR-DATA-INI          SECTION.
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
                   MOVE WRK-MES-INI-ADSAO-INDVD TO WRK-INI-MES
                   ADD 1                    TO WRK-INI-MES
                   MOVE WRK-INI-MES         TO WRK-MES-INI-ADSAO-INDVD
                   IF  WRK-MES-INI-ADSAO-INDVD
                                            GREATER 12
                       MOVE 01              TO WRK-MES-INI-ADSAO-INDVD
                       MOVE WRK-ANO-INI-ADSAO-INDVD TO WRK-INI-ANO
                       ADD 1                TO WRK-INI-ANO
                       MOVE WRK-INI-ANO TO  WRK-ANO-INI-ADSAO-INDVD
                   END-IF
               END-IF
           ELSE
               MOVE WRK-DT-PROCM-ATUAL      TO WRK-DT-INI-ADSAO-INDVD
               MOVE 01                      TO WRK-DIA-INI-ADSAO-INDVD
               MOVE WRK-MES-INI-ADSAO-INDVD TO WRK-INI-MES
               ADD 1                        TO WRK-INI-MES
               MOVE WRK-INI-MES             TO WRK-MES-INI-ADSAO-INDVD
               IF  WRK-MES-INI-ADSAO-INDVD  GREATER 12
                   MOVE 01                  TO WRK-MES-INI-ADSAO-INDVD
                   MOVE WRK-ANO-INI-ADSAO-INDVD TO WRK-INI-ANO
                   ADD 1                    TO WRK-INI-ANO
                   MOVE WRK-INI-ANO         TO WRK-ANO-INI-ADSAO-INDVD
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE FIM DA ADESAO INDIVIDUAL.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CALCULAR-DATA-FIM          SECTION.
      *----------------------------------------------------------------*

           PERFORM 3015-OBTER-DADOS-GFCTB0A2.

           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(1:2)
                                       TO WRK-DIA-FIM-ADSAO-INDVD-X.
           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(4:2)
                                       TO WRK-MES-FIM-ADSAO-INDVD-X.
           MOVE DFIM-VGCIA-TARIF       OF GFCTB0A2(7:4)
                                       TO WRK-ANO-FIM-ADSAO-INDVD-X.

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
                    PERFORM 2210-CALCULAR-DATA-FIM-ADESAO
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE FIM DA ADESAO INDIVIDUAL.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-CALCULAR-DATA-FIM-ADESAO   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-ANO-FIM-ADSAO-INDVD TO WRK-INI-ANO.
           DIVIDE WRK-INI-ANO  BY 4
                                  GIVING WRK-INTEIRO
                                  REMAINDER WRK-RESTO.

           IF WRK-MES-FIM-ADSAO-INDVD EQUAL 02 AND
              WRK-RESTO               NOT EQUAL ZEROS AND
              WRK-DIA-FIM-ADSAO-INDVD EQUAL 28
              NEXT SENTENCE
           ELSE
              MOVE WRK-MES-FIM-ADSAO-INDVD TO WRK-INI-MES
              SUBTRACT 1          FROM WRK-INI-MES
              MOVE WRK-INI-MES TO WRK-MES-FIM-ADSAO-INDVD

              EVALUATE WRK-MES-FIM-ADSAO-INDVD

              WHEN 00
                   MOVE 31        TO WRK-DIA-FIM-ADSAO-INDVD
                   MOVE 12        TO WRK-MES-FIM-ADSAO-INDVD
                   MOVE WRK-ANO-FIM-ADSAO-INDVD TO WRK-INI-ANO
                   SUBTRACT 1     FROM WRK-INI-ANO
                   MOVE WRK-INI-ANO TO WRK-ANO-FIM-ADSAO-INDVD
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
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BI0511******************************************************************
BI0511*    ROTINA ACESSAR GFCTB0A2                                     *
BI0511******************************************************************
BI0511*----------------------------------------------------------------*
BI0511 2300-ACESSAR-GFCTB0A2           SECTION.
BI0511*----------------------------------------------------------------*
BI0511
BI0511     MOVE GFCTWAI2-PACOTE
BI0511                          TO CSERVC-TARIF      OF GFCTB0A2.
BI0511     MOVE WRK-DIA-INI-ADSAO-INDVD-X
BI0511                          TO DINIC-VGCIA-TARIF OF GFCTB0A2(1:2).
BI0511     MOVE WRK-MES-INI-ADSAO-INDVD-X
BI0511                          TO DINIC-VGCIA-TARIF OF GFCTB0A2(4:2).
BI0511     MOVE WRK-ANO-INI-ADSAO-INDVD-X
BI0511                          TO DINIC-VGCIA-TARIF OF GFCTB0A2(7:4).
BI0511     MOVE '.'             TO DINIC-VGCIA-TARIF OF GFCTB0A2(3:1)
BI0511                             DINIC-VGCIA-TARIF OF GFCTB0A2(6:1).
BI0511     MOVE WRK-DIA-FIM-ADSAO-INDVD-X
BI0511                          TO DFIM-VGCIA-TARIF  OF GFCTB0A2(1:2).
BI0511     MOVE WRK-MES-FIM-ADSAO-INDVD-X
BI0511                          TO DFIM-VGCIA-TARIF  OF GFCTB0A2(4:2).
BI0511     MOVE WRK-ANO-FIM-ADSAO-INDVD-X
BI0511                          TO DFIM-VGCIA-TARIF  OF GFCTB0A2(7:4).
BI0511     MOVE '.'             TO DFIM-VGCIA-TARIF  OF GFCTB0A2(3:1)
BI0511                             DFIM-VGCIA-TARIF  OF GFCTB0A2(6:1).
BI0511
BI0511     EXEC SQL
BI0511          SELECT
BI0511               CINDCD_CREDT_CLULR
BI0511          INTO
BI0511              :GFCTB0A2.CINDCD-CREDT-CLULR
BI0511          FROM   DB2PRD.PARM_SERVC_TARIF
BI0511          WHERE  CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
BI0511            AND  DINIC_VGCIA_TARIF <= :GFCTB0A2.DINIC-VGCIA-TARIF
BI0511            AND  DFIM_VGCIA_TARIF  >= :GFCTB0A2.DFIM-VGCIA-TARIF
BI0511     END-EXEC.
BI0511
BI0511     IF (SQLCODE                NOT EQUAL ZEROS AND +100) OR
BI0511        (SQLWARN0               EQUAL 'W')
BI0511         MOVE '1318'             TO GFCTG2-COD-MSG
BI0511         MOVE 1318               TO GFCTWAI3-COD-MSG-RETORNO
BI0511         PERFORM 1500-OBTER-DESC-MSG
BI0511         MOVE 1                  TO GFCTWAI3-COD-RETORNO
BI0511                                    WRK-COD-RETORNO
BI0511         MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
BI0511         MOVE 12                 TO RETURN-CODE
BI0511         MOVE 1318               TO WRK-COD-MSG
BI0511         PERFORM 3300-INSERIR-GFCTB0L9
BI0511         MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
BI0511         MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
BI0511         MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
BI0511         PERFORM 4000-FINALIZAR
BI0511     END-IF.
BI0511
BI0511     IF  CINDCD-CREDT-CLULR OF GFCTB0A2   EQUAL   'S'
BI0511         MOVE 'S'                TO WRK-CESTA-CELULAR
BI0511     ELSE
BI0511         MOVE 'N'                TO WRK-CESTA-CELULAR
BI0511     END-IF.
BI0511
BI0511*----------------------------------------------------------------*
BI0511 2300-99-FIM.                    EXIT.
BI0511*----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA TARIFA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3010-OBTER-DADOS-GFCTB0D8       SECTION.
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
               MOVE '1317'             TO GFCTG2-COD-MSG
               MOVE 1317               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1317               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF CTPO-SERVC-TARIF OF GFCTB0D8 EQUAL 2
              NEXT SENTENCE
           ELSE
              IF CTPO-SERVC-TARIF OF GFCTB0D8 EQUAL 4
                 MOVE '1926'             TO GFCTG2-COD-MSG
                 MOVE 1926               TO GFCTWAI3-COD-MSG-RETORNO
                 PERFORM 1500-OBTER-DESC-MSG
                 MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                            WRK-COD-RETORNO
                 MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
                 MOVE 12                 TO RETURN-CODE
                 MOVE 1926               TO WRK-COD-MSG
                 PERFORM 3300-INSERIR-GFCTB0L9
                 MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
                 MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
                 MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
                 PERFORM 4000-FINALIZAR
              ELSE
                 MOVE '1926'             TO GFCTG2-COD-MSG
                 MOVE 1926               TO GFCTWAI3-COD-MSG-RETORNO
                 PERFORM 1500-OBTER-DESC-MSG
                 MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                            WRK-COD-RETORNO
                 MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
                 MOVE 12                 TO RETURN-CODE
                 MOVE 1926               TO WRK-COD-MSG
                 PERFORM 3300-INSERIR-GFCTB0L9
                 MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
                 MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
                 MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
                 PERFORM 4000-FINALIZAR
              END-IF
           END-IF.


      *----------------------------------------------------------------*
       3010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DO PARAMETRO DA TARIFA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3015-OBTER-DADOS-GFCTB0A2       SECTION.
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
               MOVE '1318'             TO GFCTG2-COD-MSG
               MOVE 1318               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1318               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3015-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IDENTIFICAR AGRUPAMENTO COM PERMISSAO DE ADESAO AO PACOTE.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3020-IDENTIFICAR-PERMISSAO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-EXISTE-AGRUPAMENTO.

           EVALUATE WRK-PROX-AGRUPAMENTO

             WHEN 004
                  IF  WRK-5527-CCGC-CPF               NOT EQUAL ZEROS
                      PERFORM 3030-ACESSAR-GFCTB0I0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 018         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 009
                  IF  WRK-1-VEZ-SEGMENTO  EQUAL  'N'
                      PERFORM 3035-OBTER-SEG-GESTAO
                      MOVE  'S'  TO  WRK-1-VEZ-SEGMENTO
                  END-IF

                  IF  GFCTJM-CSGMTO-GSTAO-TARIF       NOT EQUAL ZEROS
                      PERFORM 3040-ACESSAR-GFCTB0D2

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 'S'         TO WRK-FIM-AGRUPAMENTO
                  END-IF

             WHEN 014
                  PERFORM 3045-ACESSAR-GFCTB0C7

                  IF  WRK-EXISTE-AGRUPAMENTO          EQUAL 'S'
                      PERFORM 3075-OBTER-DADOS-GFCTB0C6
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 015         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 015
                  IF  WRK-1-VEZ-CNPJ-CPF  EQUAL  'N'
                      PERFORM 3025-OBTER-CNPJ-CPF-POSTO
                      MOVE  'S'  TO  WRK-1-VEZ-CNPJ-CPF
                  END-IF

                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3050-ACESSAR-GFCTB0D0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 017         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 017
                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3055-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 020         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 018
                  IF  WRK-1-VEZ-UF-MUNIC  EQUAL  'N'
                      PERFORM 3060-OBTER-UF-MUNICIPIO
                      MOVE  'S'  TO  WRK-1-VEZ-UF-MUNIC
                  END-IF

                  IF  GFCTRK-CMUN-RURAL               NOT EQUAL ZEROS
                      PERFORM 3065-ACESSAR-GFCTB0C9

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 019         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 019
                  IF  GFCTRK-CSGL-UF                  NOT EQUAL SPACES
                      PERFORM 3070-ACESSAR-GFCTB0D3

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 009         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 020
                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3055-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3075-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 004         TO WRK-PROX-AGRUPAMENTO
                  END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       3020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5527 PARA OBTER CPF/CNPJ/POSTO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       3025-OBTER-CNPJ-CPF-POSTO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO WRK-5527-AREA-SAIDA.

           INITIALIZE WRK-5527-AREA-SAIDA.

           MOVE 'GFCT0574'                 TO WRK-5527-CHAMADOR.
           MOVE GFCTWAI2-AGENCIA           TO WRK-5527-AGENCIA-CLIENTE
           MOVE GFCTWAI2-CONTA             TO WRK-5527-CONTA-CLIENTE.
           MOVE 'O'                        TO WRK-5527-AMBIENTE.

           MOVE 'GFCT5527'                 TO WRK-MODULO.

           CALL WRK-MODULO                 USING
                                                 WRK-5527-AREA-ENTRADA
                                                 WRK-5527-AREA-SAIDA
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE '1607'             TO GFCTG2-COD-MSG
               MOVE 1607               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1607               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  WRK-5527-COD-RETORNO        NOT EQUAL ZEROS AND 02
                                                           AND 05
               IF  WRK-5527-COD-RETORNO    EQUAL 99
                   MOVE '1608'             TO GFCTG2-COD-MSG
                   MOVE 1608               TO GFCTWAI3-COD-MSG-RETORNO
                   PERFORM 1500-OBTER-DESC-MSG
                   MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                              WRK-COD-RETORNO
                   MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
                   MOVE 12                 TO RETURN-CODE
                   MOVE 1608               TO WRK-COD-MSG
                   PERFORM 3300-INSERIR-GFCTB0L9
                   MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
                 MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
                   MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
                   PERFORM 4000-FINALIZAR
               END-IF
           END-IF.

           IF  WRK-5527-COD-RETORNO        EQUAL 02 OR 05
               MOVE ZEROS                  TO WRK-5527-CCGC-CPF
                                              WRK-5527-CFLIAL-CGC
                                              WRK-5527-CCTRL-CPF-CGC
           END-IF.

      *----------------------------------------------------------------*
       3025-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO CLIENTE.*
      ******************************************************************
      *----------------------------------------------------------------*
       3030-ACESSAR-GFCTB0I0           SECTION.
      *----------------------------------------------------------------*

           PERFORM 3031-OBTER-MAX-DATA-INICIO.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0I0.
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
ST25X6               CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST    AND
ST25X6               CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST  AND
ST25X6               CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0I0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1324'             TO GFCTG2-COD-MSG
               MOVE 1324               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1324               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                         WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0I0 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0I0 TO WRK-DT-INICIO
                   MOVE 'S'               TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3030-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAXIMA DATA DE INICIO DE PERMISSAO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3031-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0I0.
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
                      MAX(DINIC_PRMSS_PCOTE)
               INTO
                     :GFCTB0I0.DINIC-PRMSS-PCOTE
               FROM   DB2PRD.TPRMSS_GRP_CLI
              WHERE
                   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA        AND
ST25X6*               CCGC_CPF           = :GFCTB0I0.CCGC-CPF          AND
ST25X6*               CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC        AND
ST25X6*               CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC     AND
ST25X6             CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST       AND
ST25X6             CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST     AND
ST25X6             CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST  AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0I0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1324'             TO GFCTG2-COD-MSG
               MOVE 1324               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1324               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0I0
           END-IF.

      *----------------------------------------------------------------*
       3031-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5534 PARA OBTER SEGMENTO GESTAO.            *
      ******************************************************************
      *----------------------------------------------------------------*
       3035-OBTER-SEG-GESTAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJM-AREA.

           INITIALIZE GFCTJM-AREA.

           MOVE 'O'               TO GFCTJM-TPO-AMBIENTE.
           MOVE GFCTWAI2-AGENCIA  TO GFCTJM-CJUNC-DEPDC.
           MOVE GFCTWAI2-CONTA    TO GFCTJM-CCTA-CLI.

           MOVE 'GFCT5534'        TO WRK-MODULO.

           CALL WRK-MODULO        USING
                                        GFCTJM-AREA
                                        GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE '1609'             TO GFCTG2-COD-MSG
               MOVE 1609               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1609               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO      EQUAL 99
               MOVE '1610'             TO GFCTG2-COD-MSG
               MOVE 1610               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1610               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTJM-CSGMTO-GSTAO-TARIF
           END-IF.

      *----------------------------------------------------------------*
       3035-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO SEGMENTO*
      ******************************************************************
      *----------------------------------------------------------------*
       3040-ACESSAR-GFCTB0D2           SECTION.
      *----------------------------------------------------------------*

           PERFORM 3041-OBTER-MAX-DATA-INICIO.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D2.
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
                  DINIC_PRMSS_PCOTE  = :GFCTB0D2.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1328'             TO GFCTG2-COD-MSG
               MOVE 1328               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1328               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                          GREATER
                                          WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D2 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D2 TO WRK-DT-INICIO
                   MOVE 'S'               TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3040-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAXIMA DATA DE INICIO DE PERMISSAO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3041-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D2.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D2.
           MOVE GFCTJM-CSGMTO-GSTAO-TARIF
                                       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0D2.

           EXEC SQL
             SELECT
                      MAX(DINIC_PRMSS_PCOTE)
               INTO
                     :GFCTB0D2.DINIC-PRMSS-PCOTE
               FROM   DB2PRD.PRMSS_GRP_SGMTO
              WHERE
                  CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF       AND
                  CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA         AND
                  CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF AND
                  DINIC_PRMSS_PCOTE <= :GFCTB0D2.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1328'             TO GFCTG2-COD-MSG
               MOVE 1328               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1328               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D2
           END-IF.

      *----------------------------------------------------------------*
       3041-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO AGENCIA.*
      ******************************************************************
      *----------------------------------------------------------------*
       3045-ACESSAR-GFCTB0C7           SECTION.
      *----------------------------------------------------------------*

           PERFORM 3046-OBTER-MAX-DATA-INICIO.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE GFCTWAI2-AGENCIA       TO CDEPDC            OF GFCTB0C7.

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
                   DINIC_PRMSS_PCOTE  = :GFCTB0C7.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1329'             TO GFCTG2-COD-MSG
               MOVE 1329               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1329               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                          GREATER
                                          WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0C7 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C7 TO WRK-DT-INICIO
                   MOVE 'S'               TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3045-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAXIMA DATA DE INICIO DE PERMISSAO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3046-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C7.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C7.
           MOVE GFCTWAI2-AGENCIA       TO CDEPDC            OF GFCTB0C7.

           EXEC SQL
             SELECT
                      MAX(DINIC_PRMSS_PCOTE)
               INTO
                     :GFCTB0C7.DINIC-PRMSS-PCOTE
               FROM   DB2PRD.PRMSS_GRP_DEPDC
               WHERE
                   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0C7.CDEPDC)           AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0C7.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1329'             TO GFCTG2-COD-MSG
               MOVE 1329               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1329               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0C7
           END-IF.

      *----------------------------------------------------------------*
       3046-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PAB.    *
      ******************************************************************
      *----------------------------------------------------------------*
       3050-ACESSAR-GFCTB0D0           SECTION.
      *----------------------------------------------------------------*

           PERFORM 3051-OBTER-MAX-DATA-INICIO.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE GFCTWAI2-AGENCIA       TO CDEPDC            OF GFCTB0D0.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D0.

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
                   DINIC_PRMSS_PCOTE  = :GFCTB0D0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1330'             TO GFCTG2-COD-MSG
               MOVE 1330               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1330               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                          GREATER
                                          WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D0 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D0 TO WRK-DT-INICIO
                   MOVE 'S'               TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAXIMA DATA DE INICIO DE PERMISSAO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3051-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D0.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE GFCTWAI2-AGENCIA       TO CDEPDC            OF GFCTB0D0.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D0.

           EXEC SQL
             SELECT
                      MAX(DINIC_PRMSS_PCOTE)
               INTO
                     :GFCTB0D0.DINIC-PRMSS-PCOTE
               FROM   DB2PRD.PRMSS_GRP_PAB
              WHERE
                   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA        AND
                   CDEPDC             = :GFCTB0D0.CDEPDC            AND
                  (CPOSTO_SERVC       = 0                           OR
                   CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1330'             TO GFCTG2-COD-MSG
               MOVE 1330               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1330               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D0
           END-IF.

      *----------------------------------------------------------------*
       3051-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PSTAL.  *
      ******************************************************************
      *----------------------------------------------------------------*
       3055-ACESSAR-GFCTB0D1           SECTION.
      *----------------------------------------------------------------*

           PERFORM 3056-OBTER-MAX-DATA-INICIO.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE GFCTWAI2-AGENCIA       TO CDEPDC            OF GFCTB0D1.

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
                   DINIC_PRMSS_PCOTE  = :GFCTB0D1.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1331'             TO GFCTG2-COD-MSG
               MOVE 1331               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1331               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                          GREATER
                                          WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D1 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D1 TO WRK-DT-INICIO
                   MOVE 'S'               TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3055-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAXIMA DATA DE INICIO DE PERMISSAO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3056-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D1.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D1.
           MOVE GFCTWAI2-AGENCIA       TO CDEPDC            OF GFCTB0D1.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D1.

           EXEC SQL
             SELECT
                      MAX(DINIC_PRMSS_PCOTE)
               INTO
                     :GFCTB0D1.DINIC-PRMSS-PCOTE
               FROM   DB2PRD.PRMSS_GRP_PSTAL
               WHERE
                   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0D1.CDEPDC            AND
                   CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D1.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1331'             TO GFCTG2-COD-MSG
               MOVE 1331               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1331               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D1
           END-IF.

      *----------------------------------------------------------------*
       3056-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5564 PARA OBTER UF E MUNICIPIO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       3060-OBTER-UF-MUNICIPIO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA.

           INITIALIZE GFCTRK-AREA.

           MOVE 'O'                    TO GFCTRK-TPO-AMBIENTE.
           MOVE GFCTWAI2-AGENCIA       TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL WRK-MODULO             USING
                                             GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE '1611'             TO GFCTG2-COD-MSG
               MOVE 1611               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1611               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 01 OR 02 OR 99
               MOVE '1612'             TO GFCTG2-COD-MSG
               MOVE 1612               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1612               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       3060-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GP MUNICIPIO. *
      ******************************************************************
      *----------------------------------------------------------------*
       3065-ACESSAR-GFCTB0C9           SECTION.
      *----------------------------------------------------------------*

           PERFORM 3066-OBTER-MAX-DATA-INICIO.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C9.

           MOVE GFCTRK-CMUN-RURAL      TO WRK-CMUN-R.
           MOVE WRK-CMUN-7             TO CMUN-IBGE         OF GFCTB0C9.

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
                   DINIC_PRMSS_PCOTE  = :GFCTB0C9.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1334'             TO GFCTG2-COD-MSG
               MOVE 1334               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1334               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                          GREATER
                                          WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0C9 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C9 TO WRK-DT-INICIO
                   MOVE 'S'               TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3065-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAXIMA DATA DE INICIO DE PERMISSAO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3066-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C9.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
           MOVE GFCTRK-CMUN-RURAL      TO WRK-CMUN-R.
           MOVE WRK-CMUN-7             TO CMUN-IBGE         OF GFCTB0C9.

           EXEC SQL
             SELECT
                      MAX(DINIC_PRMSS_PCOTE)
               INTO
                     :GFCTB0C9.DINIC-PRMSS-PCOTE
               FROM   DB2PRD.PRMSS_GRP_MUN
              WHERE
                   CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA        AND
                   CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE         AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0C9.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1334'             TO GFCTG2-COD-MSG
               MOVE 1334               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1334               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0C9
           END-IF.

      *----------------------------------------------------------------*
       3066-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO UF.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3070-ACESSAR-GFCTB0D3           SECTION.
      *----------------------------------------------------------------*

           PERFORM 3071-OBTER-MAX-DATA-INICIO.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D3.
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
                   DINIC_PRMSS_PCOTE  = :GFCTB0D3.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1335'             TO GFCTG2-COD-MSG
               MOVE 1335               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1335               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2100-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                          GREATER
                                          WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA    OF GFCTB0D3 TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D3 TO WRK-DT-INICIO
                   MOVE 'S'               TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3070-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAXIMA DATA DE INICIO DE PERMISSAO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3071-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D3.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D3.
           MOVE GFCTRK-CSGL-UF         TO CSGL-UF           OF GFCTB0D3.

           EXEC SQL
             SELECT
                      MAX(DINIC_PRMSS_PCOTE)
               INTO
                     :GFCTB0D3.DINIC-PRMSS-PCOTE
               FROM   DB2PRD.PRMSS_GRP_UF
               WHERE
                   CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA        AND
                   CSGL_UF            = :GFCTB0D3.CSGL-UF           AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D3.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1335'             TO GFCTG2-COD-MSG
               MOVE 1335               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1335               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D3
           END-IF.

      *----------------------------------------------------------------*
       3071-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA PERMISSAO DE ADESAO AO PACOTE.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3075-OBTER-DADOS-GFCTB0C6       SECTION.
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
BI0412         AND (CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-INDVD OR
BI0412              CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-AMBAS)
               AND CIDTFD_PRMSS_INDVD  IN ( 0, 2 )
               AND DFIM_PRMSS_PCOTE    > :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1336'             TO GFCTG2-COD-MSG
               MOVE 1336               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1336               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 'S'                TO WRK-AGRUPAMENTO-VALIDO
           END-IF.

      *----------------------------------------------------------------*
       3075-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-VER-TARIFA-COMP-CAD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR
                                          WRK-EXISTE-COMPONENTE.

           PERFORM 3110-ABRIR-CURSOR.

           PERFORM 3120-LER-CURSOR.

           PERFORM 3130-PROCESSAR-CURSOR  UNTIL
                   WRK-FIM-CURSOR         EQUAL 'S' OR
                   WRK-EXISTE-COMPONENTE  EQUAL 'S'.

           PERFORM 3140-FECHAR-CURSOR.

           IF  WRK-EXISTE-COMPONENTE   EQUAL 'S'
               MOVE '1613'             TO GFCTG2-COD-MSG
               MOVE 1613               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1613               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3105-VERIFICA-PERMISSAO-ADESAO        SECTION.
      *----------------------------------------------------------------*
           MOVE GFCTWAI2-PACOTE      TO WRK-TAR-CEL-5.
           MOVE '+'                  TO WRK-TAR-CEL-S.
           MOVE WRK-TAR-CEL          TO CSERVC-TARIF     OF GFCTB0N7.
           MOVE '+'                  TO WRK-OPE-CEL-S
           MOVE GFCTWAI2-OPERADORA-N TO WRK-OPE-CEL-9.
           MOVE WRK-OPE-CEL          TO COPER-FONE-MOVEL OF GFCTB0N7.
           MOVE GFCTWAI2-DDD-N       TO WRK-DDD-CEL-3.
           MOVE '+'                  TO WRK-DDD-CEL-S.
           MOVE WRK-DDD-CEL          TO CDDD-FONE-MOVEL  OF GFCTB0N7.
           MOVE DPROCM-ATUAL                             OF GFCTB0A1 TO
                DINIC-VGCIA-REG                          OF GFCTB0N7
                DFIM-VGCIA-REG                           OF GFCTB0N7.

           EXEC SQL
             SELECT
                   QMAX_DIG_MOVEL
             INTO
                   :GFCTB0N7.QMAX-DIG-MOVEL
             FROM   DB2PRD.TOPER_PRMSS_CLULR
             WHERE CSERVC_TARIF       = :GFCTB0N7.CSERVC-TARIF
             AND   COPER_FONE_MOVEL   = :GFCTB0N7.COPER-FONE-MOVEL
             AND   CDDD_FONE_MOVEL    = :GFCTB0N7.CDDD-FONE-MOVEL
             AND   DINIC_VGCIA_REG    <= :GFCTB0N7.DINIC-VGCIA-REG
             AND   DFIM_VGCIA_REG     >= :GFCTB0N7.DFIM-VGCIA-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1856'             TO GFCTG2-COD-MSG
               MOVE 1856               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1856               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE '1857'             TO GFCTG2-COD-MSG
               MOVE 1857               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 4                  TO RETURN-CODE
               MOVE 1857               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
            END-IF.

            IF (WRK-NUM-CEL-9     EQUAL ZEROS     AND
                QMAX-DIG-MOVEL OF GFCTB0N7 = 8) OR
               (WRK-NUM-CEL-9 NOT EQUAL ZEROS     AND
                QMAX-DIG-MOVEL OF GFCTB0N7 = 9)
                NEXT SENTENCE
            ELSE
               MOVE '1858'             TO GFCTG2-COD-MSG
               MOVE 1858               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 4                  TO RETURN-CODE
               MOVE 1858               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
            END-IF.

      *----------------------------------------------------------------*
       3105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      * ABRIR CURSOR DA TABELA GFCTB029.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAI2-PACOTE   TO CTARIF-PCOTE     OF GFCTB029.

           EXEC SQL
               OPEN CSR01-GFCTB029
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1396'             TO GFCTG2-COD-MSG
               MOVE 1396               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1396               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER   CURSOR DA TABELA GFCTB029.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB029 INTO
                   :GFCTB029.CTARIF-COMPO-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1397'             TO GFCTG2-COD-MSG
               MOVE 1397               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1397               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR CURSOR DA TABELA GFCTB029.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAI2-AGENCIA  TO CJUNC-DEPDC      OF GFCTB009.
           MOVE GFCTWAI2-CONTA    TO CCTA-CLI         OF GFCTB009.
           MOVE ZEROS             TO CINDCD-EXCL-REG  OF GFCTB009.

           MOVE WRK-DIA-INI-ADSAO-INDVD-X
                                TO DFIM-ADSAO-INDVD OF GFCTB009(1:2).
           MOVE WRK-MES-INI-ADSAO-INDVD-X
                                TO DFIM-ADSAO-INDVD OF GFCTB009(4:2).
           MOVE WRK-ANO-INI-ADSAO-INDVD-X
                                TO DFIM-ADSAO-INDVD OF GFCTB009(7:4).
           MOVE '.'             TO DFIM-ADSAO-INDVD OF GFCTB009(3:1)
                                   DFIM-ADSAO-INDVD OF GFCTB009(6:1).

           EXEC SQL
             SELECT DISTINCT (B.CTARIF_PCOTE)
             INTO
                   :GFCTB029.CTARIF-PCOTE
             FROM   DB2PRD.ADSAO_INDVD_PCOTE A,
                    DB2PRD.COMPS_TARIF_PCOTE B
             WHERE
               A.CJUNC_DEPDC        = :GFCTB009.CJUNC-DEPDC       AND
               A.CCTA_CLI           = :GFCTB009.CCTA-CLI          AND
               A.CINDCD_EXCL_REG    = :GFCTB009.CINDCD-EXCL-REG   AND
               A.DFIM_ADSAO_INDVD  >= :GFCTB009.DFIM-ADSAO-INDVD  AND
               A.CSERVC_TARIF       = B.CTARIF_PCOTE              AND
               B.CTARIF_COMPO_PCOTE = :GFCTB029.CTARIF-COMPO-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1394'             TO GFCTG2-COD-MSG
               MOVE 1394               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1394               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 'S'                TO WRK-EXISTE-COMPONENTE
           END-IF.

           PERFORM 3120-LER-CURSOR.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHAR CURSOR DA TABELA GFCTB029.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3140-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB029
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1398'             TO GFCTG2-COD-MSG
               MOVE 1398               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1398               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3150-VER-TARIFA-PACOTE-CAD      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAI2-AGENCIA         TO CJUNC-DEPDC     OF GFCTB009.
           MOVE GFCTWAI2-CONTA           TO CCTA-CLI        OF GFCTB009.
           MOVE WRK-DIA-INI-ADSAO-INDVD-X
                                TO DFIM-ADSAO-INDVD OF GFCTB009(1:2).
           MOVE WRK-MES-INI-ADSAO-INDVD-X
                                TO DFIM-ADSAO-INDVD OF GFCTB009(4:2).
           MOVE WRK-ANO-INI-ADSAO-INDVD-X
                                TO DFIM-ADSAO-INDVD OF GFCTB009(7:4).
           MOVE '.'             TO DFIM-ADSAO-INDVD OF GFCTB009(3:1)
                                   DFIM-ADSAO-INDVD OF GFCTB009(6:1).
           MOVE 0               TO CINDCD-EXCL-REG  OF GFCTB009.

           EXEC SQL
             SELECT
                   DINIC_ADSAO_INDVD,
                   DFIM_ADSAO_INDVD
             INTO
                   :GFCTB009.DINIC-ADSAO-INDVD,
                   :GFCTB009.DFIM-ADSAO-INDVD
             FROM   DB2PRD.ADSAO_INDVD_PCOTE
             WHERE
                   CJUNC_DEPDC         = :GFCTB009.CJUNC-DEPDC       AND
                   CCTA_CLI            = :GFCTB009.CCTA-CLI          AND
                   CINDCD_EXCL_REG     = :GFCTB009.CINDCD-EXCL-REG   AND
                   DFIM_ADSAO_INDVD   >  :GFCTB009.DFIM-ADSAO-INDVD
           END-EXEC.

           IF (SQLCODE          NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1614'             TO GFCTG2-COD-MSG
               MOVE 1614               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1614               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF (SQLCODE              EQUAL ZEROS OR -811)
               MOVE '1614'             TO GFCTG2-COD-MSG
               MOVE 1614               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1614               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MOVIMENTAR AS INFORMACOES PARA AREA DE SAIDA.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-INSERIR-GFCTB009         SECTION.
      *----------------------------------------------------------------*
           EXEC  SQL SET :WRK-TIMESTAMP = CURRENT TIMESTAMP END-EXEC.

           MOVE GFCTWAI2-AGENCIA       TO CJUNC-DEPDC      OF GFCTB009.
           MOVE GFCTWAI2-CONTA         TO CCTA-CLI         OF GFCTB009.
           MOVE GFCTWAI2-PACOTE        TO CSERVC-TARIF     OF GFCTB009.
           MOVE 0574                   TO CFUNC-MANUT-INCL OF GFCTB009.
           MOVE ZEROS                  TO CFUNC-MANUT      OF GFCTB009.

           MOVE WRK-DIA-INI-ADSAO-INDVD-X
                                TO DINIC-ADSAO-INDVD OF GFCTB009(1:2).
           MOVE WRK-MES-INI-ADSAO-INDVD-X
                                TO DINIC-ADSAO-INDVD OF GFCTB009(4:2).
           MOVE WRK-ANO-INI-ADSAO-INDVD-X
                                TO DINIC-ADSAO-INDVD OF GFCTB009(7:4).
           MOVE '.'             TO DINIC-ADSAO-INDVD OF GFCTB009(3:1)
                                   DINIC-ADSAO-INDVD OF GFCTB009(6:1).
           MOVE WRK-DIA-FIM-ADSAO-INDVD-X
                                TO DFIM-ADSAO-INDVD  OF GFCTB009(1:2).
           MOVE WRK-MES-FIM-ADSAO-INDVD-X
                                TO DFIM-ADSAO-INDVD  OF GFCTB009(4:2).
           MOVE WRK-ANO-FIM-ADSAO-INDVD-X
                                TO DFIM-ADSAO-INDVD  OF GFCTB009(7:4).
           MOVE '.'             TO DFIM-ADSAO-INDVD  OF GFCTB009(3:1)
                                   DFIM-ADSAO-INDVD  OF GFCTB009(6:1).

           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG       OF GFCTB009.
           MOVE ZEROS                  TO CINDCD-EXCL-REG  OF GFCTB009.
           MOVE WRK-TIMESTAMP          TO HINCL-REG        OF GFCTB009.

           EXEC SQL
             INSERT INTO DB2PRD.ADSAO_INDVD_PCOTE
                  (CJUNC_DEPDC,
                   CCTA_CLI,
                   CSERVC_TARIF,
                   HINCL_REG,
                   CFUNC_MANUT_INCL,
                   CFUNC_MANUT,
                   DINIC_ADSAO_INDVD,
                   DFIM_ADSAO_INDVD,
                   HMANUT_REG,
                   CINDCD_EXCL_REG)
               VALUES
                  (:GFCTB009.CJUNC-DEPDC,
                   :GFCTB009.CCTA-CLI,
                   :GFCTB009.CSERVC-TARIF,
                   :GFCTB009.HINCL-REG,
                   :GFCTB009.CFUNC-MANUT-INCL,
                   :GFCTB009.CFUNC-MANUT,
                   :GFCTB009.DINIC-ADSAO-INDVD,
                   :GFCTB009.DFIM-ADSAO-INDVD,
                   :GFCTB009.HMANUT-REG,
                   :GFCTB009.CINDCD-EXCL-REG)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                 EQUAL 'W'      )
               MOVE '1343'              TO GFCTG2-COD-MSG
               MOVE 1343                TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE 1343               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL ZEROS
               MOVE 'S'                TO WRK-FLAG-OK
               MOVE '1615'             TO GFCTG2-COD-MSG
               MOVE 1615               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 0                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE ZEROS              TO RETURN-CODE

               MOVE DPROCM-ATUAL OF GFCTB0A1(4:2) TO WRK-MES-PROCM
               MOVE DPROCM-ATUAL OF GFCTB0A1(7:4) TO WRK-ANO-PROCM

               MOVE GFCTWAI2-DT-ABERTURA-CONTA(3:2) TO WRK-MES-ABERT
               MOVE GFCTWAI2-DT-ABERTURA-CONTA(5:4) TO WRK-ANO-ABERT
               IF WRK-DT-PROCM       NOT EQUAL WRK-DT-ABERT
                  MOVE 'N'                TO WRK-FLAG-OK
               END-IF
           END-IF.

           IF SQLCODE EQUAL -803
               MOVE '1614'             TO GFCTG2-COD-MSG
               MOVE 1614               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE 1614               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      * INSERIR CELULAR NA BASE GFCTB0M2
      ******************************************************************
      *----------------------------------------------------------------*
       3250-INSERIR-GFCTB0M2         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAI2-AGENCIA
                                       TO CAG-BCRIA        OF GFCTB0M2.
           MOVE GFCTWAI2-CONTA         TO CCTA-CLI         OF GFCTB0M2.
           MOVE GFCTWAI2-PACOTE
                                       TO CSERVC-TARIF     OF GFCTB0M2.
           MOVE 0574
                                       TO CFUNC-INCL-SIST  OF GFCTB0M2.
           MOVE ZEROS                  TO CFUNC-MANUT      OF GFCTB0M2.
           MOVE 1                      TO NSEQ-REG-CLULR   OF GFCTB0M2.
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG       OF GFCTB0M2.
           MOVE GFCTWAI2-OPERADORA-N   TO COPER-FONE-MOVEL OF GFCTB0M2.
           MOVE GFCTWAI2-DDD-N         TO CDDD-FONE-MOVEL  OF GFCTB0M2.
           MOVE WRK-NUM-CEL            TO NLIN-TFONI       OF GFCTB0M2.
           MOVE WRK-TIMESTAMP          TO HINCL-REG        OF GFCTB0M2.

           EXEC SQL
             INSERT INTO DB2PRD.TADSAO_INDVD_CLULR
                       (CAG_BCRIA,
                        CCTA_CLI,
                        CSERVC_TARIF,
                        HINCL_REG,
                        NSEQ_REG_CLULR,
                        COPER_FONE_MOVEL,
                        CDDD_FONE_MOVEL,
                        NLIN_TFONI,
                        CFUNC_INCL_SIST,
                        CFUNC_MANUT,
                        HMANUT_REG)
                VALUES (:GFCTB0M2.CAG-BCRIA,
                        :GFCTB0M2.CCTA-CLI,
                        :GFCTB0M2.CSERVC-TARIF,
                        :GFCTB0M2.HINCL-REG,
                        :GFCTB0M2.NSEQ-REG-CLULR,
                        :GFCTB0M2.COPER-FONE-MOVEL,
                        :GFCTB0M2.CDDD-FONE-MOVEL,
                        :GFCTB0M2.NLIN-TFONI,
                        :GFCTB0M2.CFUNC-INCL-SIST,
                        :GFCTB0M2.CFUNC-MANUT,
                        :GFCTB0M2.HMANUT-REG)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W'      )
               PERFORM 3800-DELETAR-ADESAO
               MOVE '1794'              TO GFCTG2-COD-MSG
               MOVE 1794                TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
                                          WRK-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 4                  TO RETURN-CODE
               MOVE 1794               TO WRK-COD-MSG
               PERFORM 3300-INSERIR-GFCTB0L9
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF WRK-FLAG-OK = 'N'
              MOVE '1603'             TO GFCTG2-COD-MSG
              MOVE 1603               TO GFCTWAI3-COD-MSG-RETORNO
              PERFORM 1500-OBTER-DESC-MSG
              MOVE 2                  TO GFCTWAI3-COD-RETORNO
                                         WRK-COD-RETORNO
              MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
              MOVE ZEROS              TO RETURN-CODE
              MOVE 1603               TO WRK-COD-MSG
              PERFORM 3300-INSERIR-GFCTB0L9
              MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
              MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
              MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
           END-IF
           .

      *----------------------------------------------------------------*
       3250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MOVIMENTACAO DA CHAVE DE IDENTIFICACAO DA LOG                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-INSERIR-GFCTB0L9         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAI2-TRANSACAO(1:8)
                                     TO  CTRANS-SOLTC-CORSP OF GFCTB0L9.
           MOVE '0574'               TO  CFNCAO-SOLTC-CORSP OF GFCTB0L9.
           MOVE 1                    TO  CAGPTO-CTA         OF GFCTB0L9.
           MOVE SPACES               TO  CIDTFD-SOLTC-CORSP OF GFCTB0L9.
           MOVE 237                  TO  CBCO               OF GFCTB0L9.
           MOVE GFCTWAI2-AGENCIA     TO  CJUNC-DEPDC        OF GFCTB0L9.
           MOVE GFCTWAI2-CONTA       TO  CCTA-BCRIA-CLI     OF GFCTB0L9.
           MOVE GFCTWAI2-PACOTE      TO  CSERVC-TARIF       OF GFCTB0L9.
           MOVE 'ABERTURA CONTAS PAN'
                                     TO  CIDTFD-SOLTC-ORIGN OF GFCTB0L9.
           MOVE 'N'                  TO  CSIT-EXTRC-REG     OF GFCTB0L9.

           IF   WRK-COD-RETORNO      EQUAL 1
                MOVE 'E'             TO  CSIT-SOLTC-CORSP   OF GFCTB0L9
           ELSE
                IF   WRK-COD-RETORNO EQUAL 2
                     MOVE 'R'        TO  CSIT-SOLTC-CORSP   OF GFCTB0L9
                END-IF
           END-IF.

           MOVE WRK-COD-MSG          TO  CIDTFD-MSGEM-RETOR OF GFCTB0L9.

           MOVE GFCTWAI2-VERSAO            TO  WRK-VERSAO
           MOVE GFCTWAI2-PAB               TO  WRK-PAB
           MOVE GFCTWAI2-SEGMENTO-CLIE     TO  WRK-SEGMENTO
           MOVE GFCTWAI2-DT-ABERTURA-CONTA TO  WRK-DT-CONTA

           MOVE LENGTH OF WRK-WLOG-REG-OCOR
                                     TO WLOG-REG-OCOR-LEN  OF GFCTB0L9.
           MOVE WRK-WLOG-REG-OCOR    TO WLOG-REG-OCOR-TEXT OF GFCTB0L9.

           EXEC SQL
             INSERT INTO DB2PRD.TLOG_SOLTC_CORSP
                  (HEFTVC_REG_SIST,
                   CTRANS_SOLTC_CORSP,
                   CFNCAO_SOLTC_CORSP,
                   CAGPTO_CTA,
                   CIDTFD_SOLTC_CORSP,
                   CBCO,
                   CJUNC_DEPDC,
                   CCTA_BCRIA_CLI,
                   CSERVC_TARIF,
                   CIDTFD_SOLTC_ORIGN,
                   CSIT_EXTRC_REG,
                   CSIT_SOLTC_CORSP,
                   CIDTFD_MSGEM_RETOR,
                   WLOG_REG_OCOR)
               VALUES
                  (CURRENT TIMESTAMP,
                   :GFCTB0L9.CTRANS-SOLTC-CORSP,
                   :GFCTB0L9.CFNCAO-SOLTC-CORSP,
                   :GFCTB0L9.CAGPTO-CTA,
                   :GFCTB0L9.CIDTFD-SOLTC-CORSP,
                   :GFCTB0L9.CBCO,
                   :GFCTB0L9.CJUNC-DEPDC,
                   :GFCTB0L9.CCTA-BCRIA-CLI,
                   :GFCTB0L9.CSERVC-TARIF,
                   :GFCTB0L9.CIDTFD-SOLTC-ORIGN,
                   :GFCTB0L9.CSIT-EXTRC-REG,
                   :GFCTB0L9.CSIT-SOLTC-CORSP,
                   :GFCTB0L9.CIDTFD-MSGEM-RETOR,
                   :GFCTB0L9.WLOG-REG-OCOR)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE '1600'             TO GFCTG2-COD-MSG
               MOVE 1600               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL ZEROS
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL -803
               MOVE '1602'             TO GFCTG2-COD-MSG
               MOVE 1602               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3500-VERIFICA-CELULAR         SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                     WAN7-ENTRADA
                                          WAN7-SAIDA
                                          WAN7-DADOS-ERRO.

           MOVE 'O'                    TO WAN7-AMBIENTE.
           MOVE GFCTWAI2-DDD-N            TO WAN7-CD-DDD.

           MOVE WRK-NUM-CEL-11         TO WAN7-NR-TELEFONE.

           MOVE 'GFCT5307'         TO     WRK-MODULO.

           CALL WRK-MODULO         USING  GFCTWAN7-REG.

           IF WAN7-COD-RETORNO  NOT EQUAL  ZEROS
               PERFORM 3600-OBTEM-MSG-GFCT5307
               PERFORM 4000-FINALIZAR
           END-IF.


      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *    ROTINA PARA TRATAR RETORNO DO MODULO GFCT5307               *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-OBTEM-MSG-GFCT5307 SECTION.
      *----------------------------------------------------------------*

           IF WAN7-COD-RETORNO  EQUAL  4
               MOVE '1783'             TO GFCTG2-COD-MSG
               MOVE 1783               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF WAN7-COD-RETORNO  EQUAL  7
               MOVE '1343'             TO GFCTG2-COD-MSG
               MOVE 1343               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF WAN7-COD-RETORNO  EQUAL  9
               MOVE '1717'             TO GFCTG2-COD-MSG
               MOVE 1717               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF WAN7-COD-RETORNO  EQUAL  11
               MOVE '1738'             TO GFCTG2-COD-MSG
               MOVE 1738               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF WAN7-COD-RETORNO  EQUAL  13
               MOVE '1787'             TO GFCTG2-COD-MSG
               MOVE 1787               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF WAN7-COD-RETORNO  EQUAL  15
               MOVE '1782'             TO GFCTG2-COD-MSG
               MOVE 1782               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF WAN7-COD-RETORNO  EQUAL  17
               MOVE '0519'             TO GFCTG2-COD-MSG
               MOVE 0519               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3600-OBTEM-MSG-GFCT5307-9-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      * CONSISTIR OPERADORA                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3700-VERIFICA-OPERADORA          SECTION.
      *----------------------------------------------------------------*


           MOVE GFCTWAI2-OPERADORA-N
                                       TO COPER-FONE-MOVEL  OF GFCTB0M4
           MOVE GFCTWAI2-DDD-N
                                       TO CDDD-FONE-MOVEL   OF GFCTB0M4

           EXEC SQL
                SELECT
                   ROPER_FONE_MOVEL
                INTO
                   :GFCTB0M4.ROPER-FONE-MOVEL
                FROM
                   DB2PRD.TOPER_FONE_CREDT
                WHERE
                    COPER_FONE_MOVEL   = :GFCTB0M4.COPER-FONE-MOVEL
                AND CDDD_FONE_MOVEL    = :GFCTB0M4.CDDD-FONE-MOVEL
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)OR
               (SQLWARN0               EQUAL 'W')
               MOVE '1797'             TO GFCTG2-COD-MSG
               MOVE 1797               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE '1798'             TO GFCTG2-COD-MSG
               MOVE 1798               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 12                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3700-99-FIM. EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA.                           *
      ******************************************************************
      ******************************************************************
      * DELETAR CELULAR POR ERRO NA INCLUSAO DE ADESAO
      ******************************************************************
      *----------------------------------------------------------------*
       3800-DELETAR-ADESAO           SECTION.
      *----------------------------------------------------------------*
           MOVE GFCTWAI2-AGENCIA       TO CJUNC-DEPDC      OF GFCTB009.
           MOVE GFCTWAI2-CONTA         TO CCTA-CLI         OF GFCTB009.
           MOVE GFCTWAI2-PACOTE        TO CSERVC-TARIF     OF GFCTB009.
           MOVE WRK-TIMESTAMP          TO HINCL-REG        OF GFCTB009.

           EXEC SQL
             DELETE FROM DB2PRD.ADSAO_INDVD_PCOTE
             WHERE
                   CJUNC_DEPDC         = :GFCTB009.CJUNC-DEPDC       AND
                   CCTA_CLI            = :GFCTB009.CCTA-CLI          AND
                   CSERVC_TARIF        = :GFCTB009.CSERVC-TARIF      AND
                   HINCL_REG           = :GFCTB009.HINCL-REG
           END-EXEC.

           IF  (SQLCODE                    NOT EQUAL ZEROS) OR
               (SQLWARN0                   EQUAL 'W')
               MOVE '1343'             TO GFCTG2-COD-MSG
               MOVE 1343               TO GFCTWAI3-COD-MSG-RETORNO
               PERFORM 1500-OBTER-DESC-MSG
               MOVE 1                  TO GFCTWAI3-COD-RETORNO
               MOVE GFCTG3-DESC-MSG    TO GFCTWAI3-COD-MSG-GFCT
               MOVE 04                 TO RETURN-CODE
               MOVE WRK-TIMESTAMP      TO GFCTWAI3-LOG-TIMESTAMP
               MOVE GFCTWAI2-TRANSACAO(1:8) TO GFCTWAI3-LOG-TRANSACAO
               MOVE GFCTWAI2-FUNCAO    TO GFCTWAI3-LOG-FUNCAO
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3800-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
