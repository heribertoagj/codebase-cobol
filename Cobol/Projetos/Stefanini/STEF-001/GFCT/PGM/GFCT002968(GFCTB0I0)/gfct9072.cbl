      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT9072.
       AUTHOR.     FRANCISCO LOURENCO.
      *================================================================*
      *            P R O C W O R K   O U T S O U R C I N G             *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT9072                                    *
      *    PROGRAMADOR.:   FRANCISCO LOURENCO      - PROCWORK /GP.50   *
      *    ANALISTA CPM:   VALERIA                 - PROCWORK /GP.50   *
      *    ANALISTA....:   MARIA VALERIA TORQUATO  - PROCWORK /GP.50   *
      *    DATA........:   18/02/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUA CONISTENCIA DO MOVIMENTO             *
      *                    DE ADESA INDIVIDUAL TOMBAMENTO              *
      *      ORIGEM                                                    *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     ENTRADA                          -         *
      *                     REJEITA                          -         *
      *                     ADESAO                           -         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                     DB2PRD.SERVC_TARIF_PRINC     GFCTB0D8      *
      *                     DB2PRD.PARM_SERVC_TARIF      GFCTB0A2      *
      *                     DB2PRD.ADSAO_INDVD_PCOTE     GFCTB009      *
      *                     DB2PRD.PARM_DATA_PROCM       GFCTB0A1      *
      *                     DB2PRD.PRMSS_ADSAO_PCOTE     GFCTB0C6      *
      *                     DB2PRD.PRMSS_GRP_DEPDC       GFCTB0C7      *
      *                     DB2PRD.PRMSS_GRP_EMPR        GFCTB0C8      *
      *                     DB2PRD.PRMSS_GRP_MUN         GFCTB0C9      *
      *                     DB2PRD.PRMSS_GRP_PAB         GFCTB0D0      *
      *                     DB2PRD.PRMSS_GRP_PSTAL       GFCTB0D1      *
      *                     DB2PRD.PRMSS_GRP_SGMTO       GFCTB0D2      *
      *                     DB2PRD.PRMSS_GRP_UF          GFCTB0D3      *
      *                     DB2PRD.TPRMSS_GRP_CLI        GFCTB0I0      *
      *                     DB2PRD.V01CLIENTE_AGENCIA    CLIEV004      *
      *                     DB2PRD.V01CTA_POUPANCA       CLIEV007      *
      *                     DB2PRD.V01CTA_CORRENTE       CLIEV008      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTRK - ENTRADA E SAIDA - OBTER COD UF E MUNICIPIO DA    *
      *               AGENCIA.                                         *
      *    I#GFCTJM - ENTRADA E SAIDA - OBTER COD SEG GESTAO TARIFA.   *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#SCCE91 - AREA DE COMUNICACAO - VALIDAR SITUACAO DA CONTA. *
      *    I#BRAD7C - AREA CHAMADA BRAD7100 - CANCELAMENTO.            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5527 - OBTER CPF/CNPJ/POSTO DE SERVICO.                 *
      *    GFCT5534 - OBTER CODIGO SEGMENTO GESTAO TARIFA.             *
      *    GFCT5564 - OBTER COD DA UF E MUNICIPIO DA AGENCIA.          *
      *    BRAD0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    BRAD1050 - FAZ CONEXAO COM DB2.                             *
      *    POOL0025 - SOMA/SUBTRAI DIAS EM DETERMINADA DATA.           *
      *    POOL1640 - SOMA DE DIAS, MESES E ANOS EM UMA DETERMINADA    *
      *               DATA COM SECULO.                                 *
      *    BRAD7100 - AREA PARA TRATAMENTO DE ERROS.                   *
      *    SCCE9100 - VERIFICAR SITUACAO DA CONTA.                     *
      *    GFCT5523 - OBTER DESCRICAO DAS MENSAGENS.                   *
      *================================================================*
151209*----------------------------------------------------------------*
151209*           SONDA PROCWORK -  CONSULTORIA - ALTERACAO            *
151209*----------------------------------------------------------------*
151209*    ANALISTA....:   MARCUS VINICIUS PINHEIRO CURTO /   GRUPO 50 *
151209*    PROGRAMADOR.:   LAYS FELIX MOLEIRO             /   GRUPO 50 *
151209*    DATA........:   15/12/2009                                  *
151209*    OBJETIVO....:   INCONSISTIR AS TARIFAS 1123 E 1124 NO       *
151209*                    PROCESSO DE ADESAO.                         *
151209*----------------------------------------------------------------*
ABR10 *----------------------------------------------------------------*
ABR10 *           SONDA PROCWORK -  CONSULTORIA - ALTERACAO            *
ABR10 *----------------------------------------------------------------*
ABR10 *    ANALISTA....:   MARCUS VINICIUS PINHEIRO CURTO /   GRUPO 50 *
ABR10 *    PROGRAMADOR.:   FABRICA SOFTWARE               /   GRUPO 50 *
ABR10 *    DATA........:   ABR/2010                                    *
ABR10 *    OBJETIVO....:   INCONSISTIR AS TARIFAS CESTA CELULAR NO     *
ABR10 *                    PROCESSO DE ADESAO.                         *
ABR10 *----------------------------------------------------------------*
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

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ENTRADA   ASSIGN     TO UT-S-ENTRADA
           FILE STATUS                 IS WRK-FS-ENTRADA.

           SELECT ADESAO   ASSIGN      TO UT-S-ADESAO
           FILE STATUS                 IS WRK-FS-ADESAO.

           SELECT REJEITA  ASSIGN      TO UT-S-REJEITA
           FILE STATUS                 IS WRK-FS-REJEITA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO CONTA CORRENTE COM TARIFA GFCT              *
      *            ORG. SEQUENCIAL     - LRECL = 060                   *
      *----------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ENTRADA.
           05  FD-AGENCIA          PIC  9(005).
           05  FD-CONTA            PIC  9(007).
           05  FD-TARIFA-GFCT      PIC  9(005).
           05  FILLER              PIC  9(005).
           05  FD-CENTRO-CUSTO     PIC  X(004).
           05  FILLER              PIC  X(034).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO REGISTROS OK                                *
      *            ORG. SEQUENCIAL     - LRECL = 60                    *
      *----------------------------------------------------------------*

       FD  ADESAO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ADESAO.
           05  FD-AGENCIA-AD       PIC  9(005).
           05  FD-CONTA-AD         PIC  9(007).
           05  FD-TARIFA-GFCT-AD   PIC  9(005).
           05  FILLER              PIC  9(005).
           05  FD-CENTRO-CUSTO-AD  PIC  X(004).
           05  FD-DATA-INIC-VIG-AD PIC  9(008).
           05  FD-DATA-FIM-VIG-AD  PIC  9(008).
           05  FD-TIPO-ATUALIZ-AD  PIC  9(001).
           05  FILLER              PIC  X(017).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO REGISTROS REJEITADOS                        *
      *            ORG. SEQUENCIAL     - LRECL = 150                   *
      *----------------------------------------------------------------*

       FD  REJEITA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REJEITADO.
           05  FD-AGENCIA-RJ           PIC  9(005).
           05  FD-CONTA-RJ             PIC  9(007).
           05  FD-TARIFA-GFCT-RJ       PIC  9(005).
           05  FILLER                  PIC  9(005).
           05  FD-CENTRO-CUSTO-RJ      PIC  X(004).
           05  FD-DATA-INIC-VIG-RJ     PIC  9(008).
           05  FD-DATA-FIM-VIG-RJ      PIC  9(008).
           05  FILLER                  PIC  X(007).
           05  FD-MENSAGEM-RJ          PIC  X(070).
           05  FILLER                  PIC  X(031).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(022)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ENTRADA           PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-ADESAO         PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-REJEITA        PIC  9(011) COMP-3  VALUE ZEROS.
       77  WRK-ACU-ATUALZ-TP-REG-1     PIC  9(011) COMP-3  VALUE ZEROS.
       77  WRK-ACU-ATUALZ-TP-REG-2     PIC  9(011) COMP-3  VALUE ZEROS.
       77  WRK-ACU-ATUALZ-TP-REG-3     PIC  9(011) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-ENTRADA              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ADESAO               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-REJEITA              PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZZZ.ZZZ.999  VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-ERRO                    PIC  X(001)         VALUE SPACES.
       01  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.

       01  WRK-COD-MSG                 PIC  9(004)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-COD-MSG.
           05  WRK-COD-MSG-X           PIC  X(004).

       01  WRK-CSERVC-TARIF            PIC S9(005) COMP-3  VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CSERVC-TARIF.
           05  WRK-COD-TARIFA          PIC  9(005) COMP-3.

       01  WRK-TIPO-PESSOA-CLIENTE     PIC  X(001)         VALUE SPACES.
       01  WRK-TIPO-CONTA-CLIENTE      PIC  9(001)         VALUE ZEROS.
       01  WRK-PROX-AGRUPAMENTO        PIC  9(003)         VALUE ZEROS.
       01  WRK-SEQ-AGPTO               PIC S9(009) COMP-3  VALUE ZEROS.
       01  WRK-DT-INICIO               PIC  X(010)         VALUE SPACES.
       01  WRK-FIM-AGRUPAMENTO         PIC  X(001)         VALUE SPACES.
       01  WRK-AGRUPAMENTO-VALIDO      PIC  X(001)         VALUE SPACES.
       01  WRK-EXISTE-AGRUPAMENTO      PIC  X(001)         VALUE SPACES.
       01  WRK-TEM-CANCELAMENTO        PIC  X(001)         VALUE SPACES.

ABR10  01  WRK-CESTA-CELULAR           PIC  X(001)         VALUE SPACES.

BI0412 01  WRK-CINDCD-ADSAO-INDVD      PIC  X(001)         VALUE 'I'.
BI0412 01  WRK-CINDCD-ADSAO-AMBAS      PIC  X(001)         VALUE 'A'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*

       01  WRK-CH-ANT.
           05  WRK-AGENCIA-ANT         PIC 9(005) VALUE 0.
           05  WRK-CONTA-ANT           PIC 9(007) VALUE 0.
           05  WRK-TARIFA-ANT          PIC 9(005) VALUE 0.

       01  WRK-CH-ATU.
           05  WRK-AGENCIA-ATU         PIC 9(005) VALUE 0.
           05  WRK-CONTA-ATU           PIC 9(007) VALUE 0.
           05  WRK-TARIFA-ATU          PIC 9(005) VALUE 0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*        AREA BRAD1640         *'.
      *----------------------------------------------------------------*

       01  WRK-1640-CAMPOS.
           05  WRK-1640-DATA-ENTRADA   PIC  X(010)         VALUE SPACES.
           05  WRK-1640-FORMATO-ENTRADA
                                       PIC  9(002)         VALUE ZEROS.
           05  WRK-1640-TIPO-INCREMENTO
                                       PIC  X(002)         VALUE SPACES.
           05  WRK-1640-INCREMENTO     PIC  9(003)         VALUE ZEROS.
           05  WRK-1640-FORMATO-SAIDA  PIC  9(002)         VALUE ZEROS.
           05  WRK-1640-DATA-SAIDA     PIC  X(010)         VALUE SPACES.
           05  WRK-1640-MENSAGEM       PIC  X(080)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*        AREA BRAD0025         *'.
      *----------------------------------------------------------------*

       01  WRK-0025-DATA-ENTRADA       PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-0025-DATA-SAIDA         PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-0025-NUMERO-DIAS        PIC S9(005) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5527 *'.
      *----------------------------------------------------------------*

       01  WRK-5527-AREA-ENTRADA.
           05  WRK-5527-AMBIENTE           PIC  X(001) VALUE ' '.
           05  WRK-5527-CHAMADOR           PIC  X(008) VALUE ' '.
           05  WRK-5527-AGENCIA-CLIENTE    PIC  9(005) VALUE 0.
           05  WRK-5527-CONTA-CLIENTE      PIC  9(013) VALUE 0.

       01  WRK-5527-AREA-SAIDA.
           05  WRK-5527-COD-RETORNO        PIC  9(002) VALUE 0.
           05  WRK-5527-COD-SQL-ERRO       PIC S9(003) VALUE 0.
           05  WRK-5527-COD-MSG-GFCT       PIC  9(004) VALUE 0.
           05  WRK-5527-DESC-MSG           PIC  X(070) VALUE ' '.
           05  WRK-5527-QTD-OCORR          PIC  9(005) VALUE 0.
           05  WRK-5527-DADOS-RETORNO.
ST25X6*      10  WRK-5527-CCGC-CPF         PIC  9(009) VALUE 0.
ST25X6       10  WRK-5527-CCGC-CPF         PIC  X(009) VALUE SPACES.
ST25X6*      10  WRK-5527-CFLIAL-CGC       PIC  9(004) VALUE 0.
ST25X6       10  WRK-5527-CFLIAL-CGC       PIC  X(004) VALUE SPACES.
             10  WRK-5527-CCTRL-CPF-CGC    PIC  9(002) VALUE 0.
             10  WRK-5527-CPOSTO-SERVC     PIC  9(003) VALUE 0.
             10  WRK-5527-CSGMTO-CLI       PIC  9(003) VALUE 0.

       01  WRK-IO-PCB.
           03  WRK-IO-TERM              PIC  X(008) VALUE ' '.
           03  FILLER                   PIC  X(002) VALUE ' '.
           03  WRK-IO-STA               PIC  X(002) VALUE ' '.
           03  FILLER                   PIC  X(012) VALUE ' '.
           03  WRK-IO-MODNAME           PIC  X(008) VALUE ' '.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME          PIC  X(008) VALUE ' '.
           03  FILLER                   PIC  X(002) VALUE ' '.
           03  WRK-ALT-STATUS           PIC  X(002) VALUE ' '.
           03  FILLER                   PIC  X(012) VALUE ' '.
           03  WRK-ALT-MODNAME          PIC  X(008) VALUE ' '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           'AREA PARA MENSAGENS'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           05  FILLER                  PIC  X(040)         VALUE
               'ERRO MODULO SCCE9100 - RETURN CODE = '.
           05  WRK-MSG01-RET-COD       PIC  X(004) VALUE ' '.

       01  WRK-MSG02.
           05  FILLER                  PIC  X(041)         VALUE
               'ERRO MODULO SCCE9100 - CODIGO RETORNO = '.
           05  WRK-MSG02-COD-RET       PIC  X(004) VALUE ' '.

       01  WRK-MSG03.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO '.
           05  WRK-MSG03-MODULO        PIC  X(008)         VALUE SPACES.

       01  WRK-MSG04.
           05  FILLER                  PIC  X(043)         VALUE
               'PARAMETRO NAO INFORMADO. PROGRAMA CANCELADO'.

       01  WRK-MSG05.
           05  FILLER                  PIC  X(050)         VALUE
               'NUMERO DE MESES NAO NUMERICO PROGRAMA CANCELADO'.

       01  WRK-MSG06.
           05  FILLER                  PIC  X(050)         VALUE
               'PARAMETRO CONTEM ZERO - PROGRAMA CANCELADO'.

       01  WRK-MSG07.
           05  FILLER                  PIC  X(040)         VALUE
               'ERRO MODULO BRAD1640 - RETURN CODE = '.
           05  WRK-MSG07-RET-COD       PIC  X(004) VALUE ' '.

       01  WRK-MSG08.
           05  FILLER                  PIC  X(040)         VALUE
               'ERRO MODULO BRAD0025 - RETURN CODE = '.
           05  WRK-MSG08-RET-COD       PIC  X(004) VALUE ' '.

       01  WRK-MSG09.
           03  FILLER                  PIC  X(040)         VALUE
               'ERRO MODULO GFCT5527 - RETURN CODE = '.
           03  WRK-MSG09-CD            PIC  X(004) VALUE ' '.

       01  WRK-MSG10.
           03  FILLER                  PIC  X(041)         VALUE
               'ERRO MODULO GFCT5527 - CODIGO RETORNO = '.
           03  WRK-MSG10-CD            PIC  X(004) VALUE ' '.

       01  WRK-MSG11.
           03  FILLER                  PIC  X(040)         VALUE
               'ERRO MODULO GFCT5534 - RETURN CODE = '.
           03  WRK-MSG11-CD            PIC  X(004) VALUE ' '.

       01  WRK-MSG12.
           03  FILLER                  PIC  X(041)         VALUE
               'ERRO MODULO GFCT5534 - CODIGO RETORNO = '.
           03  WRK-MSG12-CD            PIC  X(004) VALUE ' '.

       01  WRK-MSG13.
           03  FILLER                  PIC  X(040)         VALUE
               'ERRO MODULO GFCT5564 - RETURN CODE = '.
           03  WRK-MSG13-CD            PIC  X(004) VALUE ' '.

       01  WRK-MSG14.
           03  FILLER                  PIC  X(041)         VALUE
               'ERRO MODULO GFCT5564 - CODIGO RETORNO = '.
           03  WRK-MSG14-CD            PIC  X(004) VALUE ' '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(015)         VALUE
           'AREA PARA DATAS'.
      *----------------------------------------------------------------*

       01  WRK-DATA-AUX                PIC  9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-AUX.
           05 FILLER                   PIC  9(001).
           05 WRK-DIA-AUX              PIC  9(002).
           05 WRK-MES-AUX              PIC  9(002).
           05 WRK-ANO-AUX              PIC  9(004).

       01  WRK-DATA-NOR                PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-NOR.
           05  WRK-DIA-NOR             PIC  9(002).
           05  WRK-MES-NOR             PIC  9(002).
           05  WRK-ANO-NOR             PIC  9(004).

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

       01  WRK-DPROCM-ATUAL-INV        PIC  9(008)         VALUE ZEROS.

       01  WRK-DINIC-VGCIA-TARIF       PIC  X(010)         VALUE SPACES.

       01  WRK-DINIC-VGCIA-TARIF-INV   PIC  9(008)         VALUE ZEROS.

       01  WRK-DFIM-VGCIA-TARIF        PIC  X(010)         VALUE SPACES.

       01  WRK-DINIC-ADSAO-INDVD       PIC  X(010)         VALUE SPACES.

       01  WRK-DFIM-ADSAO-INDVD-INV    PIC  9(008)         VALUE ZEROS.

       01  WRK-DATA-INIC-AD            PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-INIC-AD.
           05  WRK-DIA-INIC-AD         PIC  9(002).
           05  WRK-MES-INIC-AD         PIC  9(002).
           05  WRK-ANO-INIC-AD         PIC  9(004).

       01  WRK-DATA-FIM-AD             PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-FIM-AD.
           05  WRK-DIA-FIM-AD          PIC  9(002).
           05  WRK-MES-FIM-AD          PIC  9(002).
           05  WRK-ANO-FIM-AD          PIC  9(004).

       01  WRK-DATA-FIM-VIG-AD-INV     PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(026)         VALUE
           'AREA DE ERRO - FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DO MODULO GFCT5523 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5534 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTJM'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5564 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTRK'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA PARA BRAD7100'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA PARA ERROS'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT0M'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM SCCE9100 *'.
      *----------------------------------------------------------------*

       COPY 'I#SCCE91'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB009
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(033)         VALUE
           'INICIO DA LINKAGE STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-PARM-AREA.
          05  LNK-PARM-TAMANHO         PIC S9(004) COMP.
          05  LNK-PARM-QTD-MES         PIC  9(003).

      *================================================================*
       PROCEDURE                       DIVISION USING LNK-PARM-AREA.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR
             UNTIL WRK-FS-ENTRADA      EQUAL '10'.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           OPEN INPUT  ENTRADA
                OUTPUT ADESAO
                       REJEITA.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 9200-TESTAR-FILE-STATUS.

           PERFORM 1100-VERIFICAR-VAZIO.

           PERFORM 1200-CONSISTIR-PARAMETRO.

           PERFORM 1300-OBTER-DATA-PROCESSAMENTO.

           PERFORM 1400-TRATAR-DATA-VIGENCIA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE ARQUIVO ENTRADA ESTA VAZIO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ENTRADA.

           IF  ACU-LIDOS-ENTRADA       EQUAL ZEROS
               DISPLAY ' '
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '*                   PROGRAMA GFCT9072
      -        '        *'
               DISPLAY '*                   -----------------
      -        '        *'
               DISPLAY '*
      -        '        *'
               DISPLAY '*               ARQUIVO DE ENTRADA VAZIO
      -        '        *'
               DISPLAY '*-----------------------------------------------
      -        '--------*'

               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE CONSISTE O PARAMETRO INFORMADO                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-CONSISTIR-PARAMETRO        SECTION.
      *----------------------------------------------------------------*

           IF  LNK-PARM-TAMANHO        EQUAL ZEROS
               DISPLAY '********** GFCT9072 **********'
               DISPLAY '*                            *'
               DISPLAY '*  PARAMETRO NAO INFORMADO   *'
               DISPLAY '*  PROGRAMA CANCELADO        *'
               DISPLAY '*                            *'
               DISPLAY '********** GFCT9072 **********'

               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG04          TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  LNK-PARM-QTD-MES        NOT NUMERIC
               DISPLAY '********** GFCT9072 **********'
               DISPLAY '*                            *'
               DISPLAY '*  MESES NAO NUMERICOS       *'
               DISPLAY '*  PROGRAMA CANCELADO        *'
               DISPLAY '*                            *'
               DISPLAY '********** GFCT9072 **********'

               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG05          TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  LNK-PARM-QTD-MES        EQUAL ZEROS
               DISPLAY '********** GFCT9072 **********'
               DISPLAY '*                            *'
               DISPLAY '*  PARAMETRO ZERADO          *'
               DISPLAY '*  PROGRAMA CANCELADO        *'
               DISPLAY '*                            *'
               DISPLAY '********** GFCT9072 **********'

               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG06          TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE LE TABELA GFCTBA1 E BUSCA DATAS PROCESSAMENTO    *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OBTER-DATA-PROCESSAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                    CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE CALCULA AS DATA DE INICIO E FIM VIGENCIA ADESAO  *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-TRATAR-DATA-VIGENCIA       SECTION.
      *----------------------------------------------------------------*

      *--- MONTA DATA DE INICIO DA VIGENCIA DA ADESAO

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-DB2.
           MOVE 01                     TO WRK-DIA-INIC-AD.
           MOVE WRK-MES-DB2            TO WRK-MES-INIC-AD.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INIC-AD.

           IF  WRK-MES-INIC-AD         EQUAL 12
               MOVE 01                 TO WRK-MES-INIC-AD
               ADD  1                  TO WRK-ANO-INIC-AD
           ELSE
               ADD  1                  TO WRK-MES-INIC-AD
           END-IF.

           MOVE WRK-DIA-INIC-AD        TO WRK-DIA-DB2.
           MOVE WRK-MES-INIC-AD        TO WRK-MES-DB2.
           MOVE WRK-ANO-INIC-AD        TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO WRK-DINIC-ADSAO-INDVD.

      *--- CALCULA/MONTA DATA DE FIM DA VIGENCIA DA ADESAO

           MOVE WRK-DIA-INIC-AD        TO WRK-DIA-DB2.
           MOVE WRK-MES-INIC-AD        TO WRK-MES-DB2.
           MOVE WRK-ANO-INIC-AD        TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO WRK-1640-DATA-ENTRADA.

           MOVE 09                     TO WRK-1640-FORMATO-ENTRADA.
           MOVE 'M'                    TO WRK-1640-TIPO-INCREMENTO.
           MOVE LNK-PARM-QTD-MES       TO WRK-1640-INCREMENTO.
           MOVE 09                     TO WRK-1640-FORMATO-SAIDA.

           CALL 'BRAD1640'             USING WRK-1640-DATA-ENTRADA
                                             WRK-1640-FORMATO-ENTRADA
                                             WRK-1640-TIPO-INCREMENTO
                                             WRK-1640-INCREMENTO
                                             WRK-1640-FORMATO-SAIDA
                                             WRK-1640-DATA-SAIDA
                                             WRK-1640-MENSAGEM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               DISPLAY '********************** GFCT9072 ***************'
                       '*******'
               DISPLAY '*                                             *'
                       '      *'
               DISPLAY '* ERRO NA BRAD1640                            *'
                       '      *'
               DISPLAY '* ' WRK-1640-MENSAGEM ' *'
               DISPLAY '*                                             *'
                       '      *'
               DISPLAY '********************** GFCT9072 ***************'
                       '*******'

               MOVE 'APL'              TO ERR-TIPO-ACESSO

               MOVE RETURN-CODE        TO WRK-MSG07-RET-COD
               MOVE WRK-MSG07          TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-1640-DATA-SAIDA    TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-FIM-AD.
           MOVE WRK-MES-DB2            TO WRK-MES-FIM-AD.
           MOVE WRK-ANO-DB2            TO WRK-ANO-FIM-AD.

      *--- MONTA DATA DE FIM DA VIGENCIA DA ADESAO

           MOVE 01                     TO WRK-DIA-NOR.
           MOVE WRK-MES-FIM-AD         TO WRK-MES-NOR.
           MOVE WRK-ANO-FIM-AD         TO WRK-ANO-NOR.

           IF  WRK-MES-NOR             EQUAL 12
               MOVE 01                 TO WRK-MES-NOR
               ADD  1                  TO WRK-ANO-NOR
           ELSE
               ADD  1                  TO WRK-MES-NOR
           END-IF.

           MOVE WRK-DATA-NOR           TO WRK-0025-DATA-ENTRADA.
           MOVE -01                    TO WRK-0025-NUMERO-DIAS.

           CALL 'BRAD0025'             USING WRK-0025-DATA-ENTRADA
                                             WRK-0025-NUMERO-DIAS
                                             WRK-0025-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               MOVE RETURN-CODE        TO WRK-MSG08-RET-COD
               MOVE WRK-MSG08          TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-0025-DATA-SAIDA    TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-FIM-AD.
           MOVE WRK-MES-AUX            TO WRK-MES-FIM-AD.
           MOVE WRK-ANO-AUX            TO WRK-ANO-FIM-AD.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-ERRO.

           IF  WRK-CH-ATU              EQUAL WRK-CH-ANT
               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE 1151               TO WRK-COD-MSG
               PERFORM 3500-GRAVAR-REJEITADOS
           ELSE
               MOVE WRK-CH-ATU         TO WRK-CH-ANT
               MOVE 'N'                TO WRK-INCONSIS
               MOVE 'N'                TO WRK-TEM-CANCELAMENTO
               PERFORM 2200-CONSISTENCIA-FISICA
               IF  WRK-ERRO            EQUAL ZEROS
                   PERFORM 2300-CONSISTENCIA-LOGICA
                   IF  WRK-ERRO        EQUAL ZEROS
                       PERFORM 3000-GRAVAR-ADESAO
                   END-IF
               END-IF
           END-IF.

           PERFORM 2100-LER-ENTRADA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA LEITURA E TRATAMENTO DO ARQUIVO DE ENTRADA*
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ENTRADA                SECTION.
      *----------------------------------------------------------------*

           READ ENTRADA.

           IF  WRK-FS-ENTRADA          EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 9210-TESTAR-FS-ENTRADA.

           ADD  1                      TO ACU-LIDOS-ENTRADA.

           MOVE FD-AGENCIA             TO WRK-AGENCIA-ATU.
           MOVE FD-CONTA               TO WRK-CONTA-ATU.
           MOVE FD-TARIFA-GFCT         TO WRK-TARIFA-ATU.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EFETUAR CONSISTENCIA FISICA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONSISTENCIA-FISICA        SECTION.
      *----------------------------------------------------------------*

           IF  FD-CENTRO-CUSTO         EQUAL SPACES
               MOVE 'S'                TO WRK-INCONSIS

               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE 1152               TO WRK-COD-MSG
           ELSE
               IF  FD-AGENCIA          NOT NUMERIC OR
                   FD-AGENCIA          EQUAL ZEROS OR
                   FD-CONTA            NOT NUMERIC OR
                   FD-CONTA            EQUAL ZEROS
                   MOVE 'S'            TO WRK-INCONSIS

                   MOVE FD-ENTRADA     TO FD-REJEITADO
                   MOVE 1153           TO WRK-COD-MSG
               ELSE
                   IF  FD-TARIFA-GFCT  NOT NUMERIC OR
                       FD-TARIFA-GFCT  EQUAL ZEROS
                       MOVE 'S'        TO WRK-INCONSIS

                       MOVE FD-ENTRADA TO FD-REJEITADO
                       MOVE 1154       TO WRK-COD-MSG
                   END-IF
               END-IF
           END-IF.

           IF  WRK-INCONSIS            EQUAL 'S'
               PERFORM 3500-GRAVAR-REJEITADOS
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EFETUAR CONSISTENCIA LOGICA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-CONSISTENCIA-LOGICA        SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-VER-TARIFA-PACOTE-CAD.

           IF  WRK-INCONSIS            EQUAL 'N'
               PERFORM 2320-CONSISTE-AG-CTA

               IF  WRK-INCONSIS        EQUAL 'N'
                   PERFORM 2330-OBTER-DADOS-GFCTB0D8

                   IF  WRK-INCONSIS    EQUAL 'N'
                       PERFORM 2340-OBTER-DADOS-GFCTB0A2

                       IF  WRK-INCONSIS
                                       EQUAL 'N'
                           PERFORM 2400-CONSISTE-PERMISSAO
                       END-IF
                   END-IF
               END-IF
           END-IF.

           IF  WRK-INCONSIS            EQUAL 'S'
               PERFORM 3500-GRAVAR-REJEITADOS
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR A EXISTENCIA DE TARIFA PACOTE JA CADASTRADA.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-VER-TARIFA-PACOTE-CAD      SECTION.
      *----------------------------------------------------------------*

           MOVE FD-AGENCIA             TO CJUNC-DEPDC       OF GFCTB009.
           MOVE FD-CONTA               TO CCTA-CLI          OF GFCTB009.

           MOVE WRK-DATA-INIC-AD       TO WRK-DATA-NOR.
           MOVE WRK-DIA-NOR            TO WRK-DIA-DB2.
           MOVE WRK-MES-NOR            TO WRK-MES-DB2.
           MOVE WRK-ANO-NOR            TO WRK-ANO-DB2.

           MOVE WRK-DATA-DB2           TO DFIM-ADSAO-INDVD  OF GFCTB009.

           MOVE 0                      TO CINDCD-EXCL-REG   OF GFCTB009.

           EXEC SQL
             SELECT
                   CSERVC_TARIF     ,
                   DINIC_ADSAO_INDVD,
                   DFIM_ADSAO_INDVD
             INTO
                   :GFCTB009.CSERVC-TARIF     ,
                   :GFCTB009.DINIC-ADSAO-INDVD,
                   :GFCTB009.DFIM-ADSAO-INDVD
             FROM   DB2PRD.ADSAO_INDVD_PCOTE
             WHERE
                   CJUNC_DEPDC          = :GFCTB009.CJUNC-DEPDC      AND
                   CCTA_CLI             = :GFCTB009.CCTA-CLI         AND
                   CINDCD_EXCL_REG      = :GFCTB009.CINDCD-EXCL-REG  AND
                   DFIM_ADSAO_INDVD    >  :GFCTB009.DFIM-ADSAO-INDVD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_INDVD_PCOTE'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL -811
               MOVE 'S'                TO WRK-INCONSIS

               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE 1161               TO WRK-COD-MSG
           ELSE
               IF  SQLCODE             EQUAL ZEROS
                   IF  CSERVC-TARIF    OF GFCTB009
                                       EQUAL FD-TARIFA-GFCT
                       MOVE 'S'        TO WRK-INCONSIS

                       MOVE FD-ENTRADA TO FD-REJEITADO
                       MOVE 1162       TO WRK-COD-MSG
                   ELSE
                       MOVE 'S'        TO WRK-TEM-CANCELAMENTO
                   END-IF
               END-IF
           END-IF.

151209*****IF  FD-TARIFA-GFCT  EQUAL 01123 OR 01124
151209*********MOVE 'S'        TO WRK-INCONSIS
151209*********MOVE FD-ENTRADA TO FD-REJEITADO
151209*********MOVE 1482       TO WRK-COD-MSG
151209*****END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMA MODULO SCCE9100 PARA VERIFICAR SE A AGENCIA/CONTA EH     *
      * VALIDA.                                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-CONSISTE-AG-CTA            SECTION.
      *----------------------------------------------------------------*

           MOVE FD-AGENCIA             TO LKW-CJUNC-DEPC.
           MOVE FD-CONTA               TO LKW-CTA-CORR.

           MOVE 'SCCE9100'             TO WRK-MODULO.

           CALL 'BRAD0081'             USING WRK-MODULO
                                             LKW-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-INCONSIS

               MOVE 'MSGX'             TO WRK-COD-MSG-X

               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE RETURN-CODE        TO WRK-MSG01-RET-COD
               MOVE WRK-MSG01          TO FD-MENSAGEM-RJ
           ELSE
               IF  LKW-CD-RETORNO      NOT EQUAL ZEROS
                   MOVE 'S'            TO WRK-INCONSIS

                   MOVE 'MSGX'         TO WRK-COD-MSG-X

                   MOVE FD-ENTRADA     TO FD-REJEITADO
                   MOVE LKW-CD-RETORNO TO WRK-MSG02-COD-RET
                   MOVE WRK-MSG02      TO FD-MENSAGEM-RJ
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA TARIFA PACOTE                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-OBTER-DADOS-GFCTB0D8       SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0D8.
           MOVE 2                      TO CTPO-SERVC-TARIF  OF GFCTB0D8.

           EXEC SQL
             SELECT
                   CPSSOA_SERVC_TARIF,
                   CTPO_CTA_DEB_TARIF
             INTO
                   :GFCTB0D8.CPSSOA-SERVC-TARIF,
                   :GFCTB0D8.CTPO-CTA-DEB-TARIF
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF         = :GFCTB0D8.CSERVC-TARIF     AND
                   CTPO_SERVC_TARIF     = :GFCTB0D8.CTPO-SERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-INCONSIS

               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE 1158               TO WRK-COD-MSG
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER VIGENCIA DA TARIFA PACOTE                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-OBTER-DADOS-GFCTB0A2       SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL           TO WRK-DINIC-VGCIA-TARIF
                                          WRK-DFIM-VGCIA-TARIF.
           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
             SELECT
                   DINIC_VGCIA_TARIF,
                   DFIM_VGCIA_TARIF
             INTO
                   :GFCTB0A2.DINIC-VGCIA-TARIF,
                   :GFCTB0A2.DFIM-VGCIA-TARIF
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF         = :GFCTB0A2.CSERVC-TARIF     AND
                  (DINIC_VGCIA_TARIF   <= :WRK-DINIC-VGCIA-TARIF     AND
                   DFIM_VGCIA_TARIF    >  :WRK-DFIM-VGCIA-TARIF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_SERVC_TARIF' TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2345-OBTER-GFCTB0A2-FUTURA
           ELSE
               MOVE DINIC-VGCIA-TARIF OF GFCTB0A2
                                       TO WRK-DINIC-VGCIA-TARIF
               MOVE DFIM-VGCIA-TARIF OF GFCTB0A2
                                       TO WRK-DFIM-VGCIA-TARIF
           END-IF.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER VIGENCIA DA TARIFA PACOTE FUTURA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2345-OBTER-GFCTB0A2-FUTURA      SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL           TO WRK-DINIC-VGCIA-TARIF.
           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
             SELECT
                   DINIC_VGCIA_TARIF,
                   DFIM_VGCIA_TARIF
             INTO
                   :GFCTB0A2.DINIC-VGCIA-TARIF,
                   :GFCTB0A2.DFIM-VGCIA-TARIF
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF         = :GFCTB0A2.CSERVC-TARIF     AND
                   DINIC_VGCIA_TARIF   >  :WRK-DINIC-VGCIA-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_SERVC_TARIF' TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-INCONSIS

               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE 1160               TO WRK-COD-MSG
           ELSE
               MOVE DINIC-VGCIA-TARIF OF GFCTB0A2
                                       TO WRK-DINIC-VGCIA-TARIF
               MOVE DFIM-VGCIA-TARIF OF GFCTB0A2
                                       TO WRK-DFIM-VGCIA-TARIF
           END-IF.

      *----------------------------------------------------------------*
       2345-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTIR PERMISSAO                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-CONSISTE-PERMISSAO         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-ACESSAR-CLIEV008.

           IF  WRK-INCONSIS            EQUAL 'N'
               IF  CTPO-CTA-DEB-TARIF OF GFCTB0D8
                                       EQUAL WRK-TIPO-CONTA-CLIENTE
                   IF  CPSSOA-SERVC-TARIF OF GFCTB0D8
                                       EQUAL 3    OR
                      (CPSSOA-SERVC-TARIF OF GFCTB0D8
                                       EQUAL 1    AND
                       WRK-TIPO-PESSOA-CLIENTE
                                       EQUAL 'F') OR
                      (CPSSOA-SERVC-TARIF OF GFCTB0D8
                                       EQUAL 2    AND
                       WRK-TIPO-PESSOA-CLIENTE
                                       EQUAL 'J')
                       MOVE 014        TO WRK-PROX-AGRUPAMENTO

                       MOVE 'N'        TO WRK-FIM-AGRUPAMENTO

                       MOVE 'N'        TO WRK-AGRUPAMENTO-VALIDO

                       PERFORM 2500-IDENTIFICAR-PERMISSAO
                         UNTIL WRK-FIM-AGRUPAMENTO
                                       EQUAL 'S'
                            OR WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'S'
                            OR WRK-INCONSIS
                                       EQUAL 'S'

                       IF  WRK-INCONSIS
                                       EQUAL 'N'
                           IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                               MOVE 'S'
                                       TO WRK-INCONSIS

                               MOVE FD-ENTRADA
                                       TO FD-REJEITADO
                               MOVE 1164
                                       TO WRK-COD-MSG
ABR10                      ELSE
BI0511                         PERFORM 2440-ACESSAR-GFCTB0A2
ABR10                          IF  WRK-CESTA-CELULAR   EQUAL 'S'
ABR10                              MOVE 'S'
ABR10                                  TO WRK-INCONSIS
ABR10
ABR10                              MOVE FD-ENTRADA
ABR10                                  TO FD-REJEITADO
ABR10                              MOVE 1482
ABR10                                  TO WRK-COD-MSG
ABR10                          END-IF
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   MOVE 'S'            TO WRK-INCONSIS

                   MOVE FD-ENTRADA     TO FD-REJEITADO
                   MOVE 1165           TO WRK-COD-MSG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV008                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-ACESSAR-CLIEV008           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-AGENCIA             TO CJUNC-DEPDC      OF CLIEV008.
           MOVE FD-CONTA               TO CCTA-CLI         OF CLIEV008.

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
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2420-ACESSAR-CLIEV007
           ELSE
               MOVE 1                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV008
                                       TO CID-CLI          OF CLIEV004

               PERFORM 2430-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSAR DADOS DO CLIENTE                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-ACESSAR-CLIEV007           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-AGENCIA             TO CJUNC-DEPDC       OF CLIEV007.
           MOVE FD-CONTA               TO CCTA-CLI          OF CLIEV007.

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
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-INCONSIS

               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE 1163               TO WRK-COD-MSG
           ELSE
               MOVE 2                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV007
                                       TO CID-CLI          OF CLIEV004

               PERFORM 2430-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV004 PARA OBTER DADOS DO CLIENTE                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-ACESSAR-CLIEV004           SECTION.
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
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-INCONSIS

               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE 1163               TO WRK-COD-MSG
           ELSE
               IF  CFLIAL-CGC OF CLIEV004
                                       EQUAL ZEROS
                   MOVE 'F'            TO WRK-TIPO-PESSOA-CLIENTE
               ELSE
                   MOVE 'J'            TO WRK-TIPO-PESSOA-CLIENTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BI0511******************************************************************
BI0511*    ROTINA ACESSAR GFCTB0A2                                     *
BI0511******************************************************************
BI0511*----------------------------------------------------------------*
BI0511 2440-ACESSAR-GFCTB0A2       SECTION.
BI0511*----------------------------------------------------------------*
BI0511
BI0511     MOVE FD-TARIFA-GFCT    TO CSERVC-TARIF        OF GFCTB0A2.
BI0511
BI0511     MOVE DPROCM-ATUAL      OF GFCTB0A1
BI0511                            TO DINIC-VGCIA-TARIF   OF GFCTB0A2.
BI0511
BI0511     MOVE DPROCM-ATUAL      OF GFCTB0A1
BI0511                            TO DFIM-VGCIA-TARIF    OF GFCTB0A2.
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
BI0511          MOVE 'DB2'                TO ERR-TIPO-ACESSO
BI0511          MOVE 'PARM_SERVC_TARIF'
BI0511                                    TO ERR-DBD-TAB
BI0511          MOVE 'SELECT'             TO ERR-FUN-COMANDO
BI0511          MOVE  SQLCODE             TO ERR-SQL-CODE
BI0511          MOVE '0170'               TO ERR-LOCAL
BI0511          MOVE  SPACES              TO ERR-SEGM
BI0511          PERFORM 9999-PROCESSAR-ROTINA-ERRO
BI0511     END-IF.
BI0511
BI0511     IF  CINDCD-CREDT-CLULR OF GFCTB0A2   EQUAL   'S'
BI0511         MOVE 'S'                TO WRK-CESTA-CELULAR
BI0511     ELSE
BI0511         MOVE 'N'                TO WRK-CESTA-CELULAR
BI0511     END-IF.
BI0511
BI0511*----------------------------------------------------------------*
BI0511 2440-99-FIM.                    EXIT.
BI0511*----------------------------------------------------------------*

      ******************************************************************
      * IDENTIFICAR AGRUPAMENTO COM PERMISSAO DE ADESAO AO PACOTE.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-IDENTIFICAR-PERMISSAO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-EXISTE-AGRUPAMENTO.

           EVALUATE WRK-PROX-AGRUPAMENTO

             WHEN 004
                  PERFORM 2520-OBTER-CNPJ-CPF-POSTO

                  IF  WRK-5527-CCGC-CPF
                                       NOT EQUAL ZEROS
                      PERFORM 2525-ACESSAR-GFCTB0I0

                      IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                          PERFORM 2510-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                      MOVE 018         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 009
                  PERFORM 2530-OBTER-SEG-GESTAO

                  IF  GFCTJM-CSGMTO-GSTAO-TARIF
                                       NOT EQUAL ZEROS
                      PERFORM 2535-ACESSAR-GFCTB0D2

                      IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                          PERFORM 2510-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                      MOVE 'S'         TO WRK-FIM-AGRUPAMENTO
                  END-IF

             WHEN 014
                  PERFORM 2540-ACESSAR-GFCTB0C7

                  IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                      PERFORM 2510-OBTER-DADOS-GFCTB0C6
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                      MOVE 015         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 015
                  PERFORM 2520-OBTER-CNPJ-CPF-POSTO

                  IF  WRK-5527-CPOSTO-SERVC
                                       NOT EQUAL ZEROS
                      PERFORM 2550-ACESSAR-GFCTB0D0

                      IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                          PERFORM 2510-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                      MOVE 017         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 017
                  IF  WRK-5527-CPOSTO-SERVC
                                       NOT EQUAL ZEROS
                      PERFORM 2560-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                          PERFORM 2510-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                      MOVE 020         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 018
                  PERFORM 2570-OBTER-UF-MUNICIPIO

                  IF  GFCTRK-CMUN-RURAL
                                       NOT EQUAL ZEROS
                      PERFORM 2575-ACESSAR-GFCTB0C9

                      IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                          PERFORM 2510-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                      MOVE 019         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 019
                  IF  GFCTRK-CSGL-UF   NOT EQUAL SPACES
                      PERFORM 2580-ACESSAR-GFCTB0D3

                      IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                          PERFORM 2510-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                      MOVE 009         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 020
                  IF  WRK-5527-CPOSTO-SERVC
                                       NOT EQUAL ZEROS
                      PERFORM 2560-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                          PERFORM 2510-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                      MOVE 004         TO WRK-PROX-AGRUPAMENTO
                  END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA PERMISSAO DE ADESAO AO PACOTE.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-OBTER-DADOS-GFCTB0C6       SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE WRK-SEQ-AGPTO          TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
           MOVE WRK-DT-INICIO          TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE DPROCM-PROX            OF GFCTB0A1
                                       TO DFIM-PRMSS-PCOTE  OF GFCTB0C6.

BI0412     MOVE 'I'                    TO WRK-CINDCD-ADSAO-INDVD.
BI0412     MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C6.

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
               AND CIDTFD_PRMSS_INDVD  IN (0, 3)
               AND DFIM_PRMSS_PCOTE    > :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PRMSS_ADSAO_PCOTE'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0090'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 'S'                TO WRK-AGRUPAMENTO-VALIDO
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5527 PARA OBTER CPF/CNPJ/POSTO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-OBTER-CNPJ-CPF-POSTO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-5527-AREA-SAIDA.

           INITIALIZE WRK-5527-AREA-SAIDA.

           MOVE 'GFCT1570'             TO WRK-5527-CHAMADOR.
           MOVE FD-AGENCIA             TO WRK-5527-AGENCIA-CLIENTE.
           MOVE FD-CONTA               TO WRK-5527-CONTA-CLIENTE.
           MOVE 'B'                    TO WRK-5527-AMBIENTE.

           MOVE 'GFCT5527'             TO WRK-MODULO.

           CALL 'BRAD0081'             USING WRK-MODULO
                                             WRK-5527-AREA-ENTRADA
                                             WRK-5527-AREA-SAIDA
                                             GFCT0M-AREA-ERROS
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO WRK-5527-CCGC-CPF
                                          WRK-5527-CFLIAL-CGC
                                          WRK-5527-CCTRL-CPF-CGC

               MOVE 'S'                TO WRK-INCONSIS

               MOVE 'MSGX'             TO WRK-COD-MSG-X

               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE RETURN-CODE        TO WRK-MSG09-CD
               MOVE WRK-MSG09          TO FD-MENSAGEM-RJ
           ELSE
               IF  WRK-5527-COD-RETORNO
                                       NOT EQUAL ZEROS AND 02
                                                       AND 05
                   MOVE ZEROS          TO WRK-5527-CCGC-CPF
                                          WRK-5527-CFLIAL-CGC
                                          WRK-5527-CCTRL-CPF-CGC

                   MOVE 'S'            TO WRK-INCONSIS

                   MOVE 'MSGX'         TO WRK-COD-MSG-X

                   MOVE FD-ENTRADA     TO FD-REJEITADO
                   MOVE WRK-5527-COD-RETORNO
                                       TO WRK-MSG10-CD
                   MOVE WRK-MSG10      TO FD-MENSAGEM-RJ
               ELSE
                   IF  WRK-5527-COD-RETORNO
                                       EQUAL 02 OR 05
                       MOVE ZEROS      TO WRK-5527-CCGC-CPF
                                          WRK-5527-CFLIAL-CGC
                                          WRK-5527-CCTRL-CPF-CGC
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO CLIENTE.*
      ******************************************************************
      *----------------------------------------------------------------*
       2525-ACESSAR-GFCTB0I0           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE WRK-DINIC-ADSAO-INDVD  TO DINIC-PRMSS-PCOTE OF GFCTB0I0.
           MOVE WRK-5527-CCGC-CPF      TO CCGC-CPF          OF GFCTB0I0.
           MOVE WRK-5527-CFLIAL-CGC    TO CFLIAL-CGC        OF GFCTB0I0.
           MOVE WRK-5527-CCTRL-CPF-CGC TO CCTRL-CPF-CGC     OF GFCTB0I0.

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
                   CCGC_CPF           = :GFCTB0I0.CCGC-CPF          AND
                   CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC        AND
                   CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC     AND
                   DINIC_PRMSS_PCOTE  IN
                (SELECT MAX(DINIC_PRMSS_PCOTE)
                 FROM   DB2PRD.TPRMSS_GRP_CLI
                 WHERE
                   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA        AND
                   CCGC_CPF           = :GFCTB0I0.CCGC-CPF          AND
                   CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC        AND
                   CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC     AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0I0.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TPRMSS_GRP_CLI'   TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0100'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0I0
                                       TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0I0
                                       TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       2525-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5534 PARA OBTER SEGMENTO GESTAO.            *
      ******************************************************************
      *----------------------------------------------------------------*
       2530-OBTER-SEG-GESTAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJM-AREA.

           INITIALIZE GFCTJM-AREA.

           MOVE 'B'                    TO GFCTJM-TPO-AMBIENTE.
           MOVE FD-AGENCIA             TO GFCTJM-CJUNC-DEPDC.
           MOVE FD-CONTA               TO GFCTJM-CCTA-CLI.

           MOVE 'GFCT5534'             TO WRK-MODULO.

           CALL 'BRAD0081'             USING WRK-MODULO
                                             GFCTJM-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTJM-CSGMTO-GSTAO-TARIF

               MOVE 'S'                TO WRK-INCONSIS

               MOVE 'MSGX'             TO WRK-COD-MSG-X

               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE RETURN-CODE        TO WRK-MSG11-CD
               MOVE WRK-MSG11          TO FD-MENSAGEM-RJ
           ELSE
               IF  GFCTJM-COD-RETORNO  EQUAL 99
                   MOVE ZEROS          TO GFCTJM-CSGMTO-GSTAO-TARIF

                   MOVE 'S'            TO WRK-INCONSIS

                   MOVE 'MSGX'         TO WRK-COD-MSG-X

                   MOVE FD-ENTRADA     TO FD-REJEITADO
                   MOVE GFCTJM-COD-RETORNO
                                       TO WRK-MSG12-CD
                   MOVE WRK-MSG12      TO FD-MENSAGEM-RJ
               ELSE
                   IF  GFCTJM-COD-RETORNO
                                       NOT EQUAL ZEROS
                       MOVE ZEROS      TO GFCTJM-CSGMTO-GSTAO-TARIF
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO SEGMENTO*
      ******************************************************************
      *----------------------------------------------------------------*
       2535-ACESSAR-GFCTB0D2           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE WRK-DINIC-ADSAO-INDVD  TO DINIC-PRMSS-PCOTE OF GFCTB0D2.
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
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PRMSS_GRP_SGMTO'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0110'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D2
                                       TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D2
                                       TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       2535-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO AGENCIA.*
      ******************************************************************
      *----------------------------------------------------------------*
       2540-ACESSAR-GFCTB0C7           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE WRK-DINIC-ADSAO-INDVD  TO DINIC-PRMSS-PCOTE OF GFCTB0C7.

           MOVE FD-AGENCIA             TO CDEPDC            OF GFCTB0C7

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
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PRMSS_GRP_DEPDC'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0120'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0C7
                                       TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0C7
                                       TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PAB.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2550-ACESSAR-GFCTB0D0           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE WRK-DINIC-ADSAO-INDVD  TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE FD-AGENCIA             TO CDEPDC            OF GFCTB0D0.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D0

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
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PRMSS_GRP_PAB'    TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0130'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D0 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D0 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       2550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PSTAL.  *
      ******************************************************************
      *----------------------------------------------------------------*
       2560-ACESSAR-GFCTB0D1           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE WRK-DINIC-ADSAO-INDVD  TO DINIC-PRMSS-PCOTE OF GFCTB0D1.
           MOVE FD-AGENCIA             TO CDEPDC            OF GFCTB0D1.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D1

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
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PRMSS_GRP_PSTAL'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0140'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D1 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D1 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       2560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5564 PARA OBTER UF E MUNICIPIO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       2570-OBTER-UF-MUNICIPIO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA.

           INITIALIZE GFCTRK-AREA.

           MOVE 'B'                    TO GFCTRK-TPO-AMBIENTE.
           MOVE FD-AGENCIA             TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL 'BRAD0081'             USING WRK-MODULO
                                             GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-INCONSIS

               MOVE 'MSGX'             TO WRK-COD-MSG-X

               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE RETURN-CODE        TO WRK-MSG13-CD
               MOVE WRK-MSG13          TO FD-MENSAGEM-RJ
           ELSE
               IF  GFCTRK-COD-RETORNO  EQUAL 01 OR 02 OR 99
                   MOVE 'S'            TO WRK-INCONSIS

                   MOVE 'MSGX'         TO WRK-COD-MSG-X

                   MOVE FD-ENTRADA     TO FD-REJEITADO
                   MOVE GFCTRK-COD-RETORNO
                                       TO WRK-MSG14-CD
                   MOVE WRK-MSG14      TO FD-MENSAGEM-RJ
               END-IF
           END-IF.

           IF  GFCTRK-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       2570-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GP MUNICIPIO. *
      ******************************************************************
      *----------------------------------------------------------------*
       2575-ACESSAR-GFCTB0C9           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE WRK-DINIC-ADSAO-INDVD  TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
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
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PRMSS_GRP_MUN'    TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0150'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0C9
                                       TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0C9
                                       TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       2575-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO UF.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2580-ACESSAR-GFCTB0D3           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE WRK-DINIC-ADSAO-INDVD  TO DINIC-PRMSS-PCOTE OF GFCTB0D3.
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
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PRMSS_GRP_UF'     TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0160'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D3
                                       TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D3
                                       TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       2580-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  GRAVAR ARQUIVO ADESAO                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-GRAVAR-ADESAO              SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TEM-CANCELAMENTO    EQUAL 'S'
               PERFORM 3010-TRATAR-CANCELAMENTO
           END-IF.

           MOVE FD-ENTRADA             TO FD-ADESAO.

           MOVE WRK-DATA-INIC-AD       TO FD-DATA-INIC-VIG-AD.
           MOVE WRK-DATA-FIM-AD        TO FD-DATA-FIM-VIG-AD.

           MOVE 3                      TO FD-TIPO-ATUALIZ-AD.

           WRITE FD-ADESAO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 9220-TESTAR-FS-ADESAO.

           ADD  1                      TO WRK-ACU-ATUALZ-TP-REG-3.

           ADD  1                      TO ACU-GRAVADOS-ADESAO.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR INFORMACAO PARA CANCELAMENTO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3010-TRATAR-CANCELAMENTO        SECTION.
      *----------------------------------------------------------------*

      *--- REGISTRO PARA CANCELAMENTO DA ADESAO VIGENTE

           MOVE FD-ENTRADA             TO FD-ADESAO.

           MOVE CSERVC-TARIF OF GFCTB009
                                       TO WRK-CSERVC-TARIF.
           MOVE WRK-COD-TARIFA         TO FD-TARIFA-GFCT-AD.

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DPROCM-ATUAL-INV.

           MOVE WRK-DINIC-VGCIA-TARIF  TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DINIC-VGCIA-TARIF-INV.

           IF  WRK-DINIC-VGCIA-TARIF-INV
                                       GREATER WRK-DPROCM-ATUAL-INV
               MOVE WRK-DINIC-VGCIA-TARIF
                                       TO WRK-DATA-DB2
               MOVE WRK-DIA-DB2        TO WRK-DIA-NOR,
                                          WRK-DIA-INV
               MOVE WRK-MES-DB2        TO WRK-MES-NOR,
                                          WRK-MES-INV
               MOVE WRK-ANO-DB2        TO WRK-ANO-NOR,
                                          WRK-ANO-INV
               MOVE WRK-DATA-NOR       TO FD-DATA-INIC-VIG-AD,
                                          FD-DATA-FIM-VIG-AD
           ELSE
               MOVE DINIC-ADSAO-INDVD OF GFCTB009
                                       TO WRK-DATA-DB2
               MOVE WRK-DIA-DB2        TO WRK-DIA-NOR
               MOVE WRK-MES-DB2        TO WRK-MES-NOR
               MOVE WRK-ANO-DB2        TO WRK-ANO-NOR
               MOVE WRK-DATA-NOR       TO FD-DATA-INIC-VIG-AD

               MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-DB2
               MOVE 01                 TO WRK-DIA-NOR
               MOVE WRK-MES-DB2        TO WRK-MES-NOR
               MOVE WRK-ANO-DB2        TO WRK-ANO-NOR

               IF  WRK-MES-NOR         EQUAL 12
                   MOVE 01             TO WRK-MES-NOR
                   ADD  1              TO WRK-ANO-NOR
               ELSE
                   ADD  1              TO WRK-MES-NOR
               END-IF

               MOVE WRK-DATA-NOR       TO WRK-0025-DATA-ENTRADA
               MOVE -01                TO WRK-0025-NUMERO-DIAS

               CALL 'BRAD0025'         USING WRK-0025-DATA-ENTRADA
                                             WRK-0025-NUMERO-DIAS
                                             WRK-0025-DATA-SAIDA

               IF  RETURN-CODE         NOT EQUAL ZEROS
                   MOVE 'APL'          TO ERR-TIPO-ACESSO

                   MOVE RETURN-CODE    TO WRK-MSG08-RET-COD
                   MOVE WRK-MSG08      TO ERR-TEXTO

                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF

               MOVE WRK-0025-DATA-SAIDA
                                       TO WRK-DATA-AUX
               MOVE WRK-DIA-AUX        TO WRK-DIA-NOR,
                                          WRK-DIA-INV
               MOVE WRK-MES-AUX        TO WRK-MES-NOR,
                                          WRK-MES-INV
               MOVE WRK-ANO-AUX        TO WRK-ANO-NOR,
                                          WRK-ANO-INV
               MOVE WRK-DATA-NOR       TO FD-DATA-FIM-VIG-AD

           END-IF.

           MOVE WRK-DATA-INV           TO WRK-DATA-FIM-VIG-AD-INV.

           MOVE 1                      TO FD-TIPO-ATUALIZ-AD.

           WRITE FD-ADESAO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 9220-TESTAR-FS-ADESAO.

           ADD  1                      TO WRK-ACU-ATUALZ-TP-REG-1.

           ADD  1                      TO ACU-GRAVADOS-ADESAO.

      *--- REGISTRO PARA HISTORICO DO CANCELAMENTO

           IF  WRK-DINIC-VGCIA-TARIF-INV
                                       NOT GREATER WRK-DPROCM-ATUAL-INV
               MOVE DFIM-ADSAO-INDVD OF GFCTB009
                                       TO WRK-DATA-DB2
               MOVE WRK-DIA-DB2        TO WRK-DIA-INV
               MOVE WRK-MES-DB2        TO WRK-MES-INV
               MOVE WRK-ANO-DB2        TO WRK-ANO-INV
               MOVE WRK-DATA-INV       TO WRK-DFIM-ADSAO-INDVD-INV

               IF  WRK-DATA-FIM-VIG-AD-INV
                                       LESS WRK-DFIM-ADSAO-INDVD-INV

                   MOVE FD-ENTRADA     TO FD-ADESAO

                   MOVE CSERVC-TARIF OF GFCTB009
                                       TO WRK-CSERVC-TARIF
                   MOVE WRK-COD-TARIFA TO FD-TARIFA-GFCT-AD

                   MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-DB2
                   MOVE 01             TO WRK-DIA-NOR
                   MOVE WRK-MES-DB2    TO WRK-MES-NOR
                   MOVE WRK-ANO-DB2    TO WRK-ANO-NOR

                   IF  WRK-MES-NOR     EQUAL 12
                       MOVE 01         TO WRK-MES-NOR
                       ADD  1              TO WRK-ANO-NOR
                   ELSE
                       ADD  1          TO WRK-MES-NOR
                   END-IF

                   MOVE WRK-DATA-NOR   TO FD-DATA-INIC-VIG-AD

                   MOVE DFIM-ADSAO-INDVD OF GFCTB009
                                       TO WRK-DATA-DB2
                   MOVE WRK-DIA-DB2    TO WRK-DIA-NOR
                   MOVE WRK-MES-DB2    TO WRK-MES-NOR
                   MOVE WRK-ANO-DB2    TO WRK-ANO-NOR
                   MOVE WRK-DATA-NOR   TO FD-DATA-FIM-VIG-AD

                   MOVE 2              TO FD-TIPO-ATUALIZ-AD

                   WRITE FD-ADESAO

                   MOVE WRK-GRAVACAO   TO WRK-OPERACAO

                   PERFORM 9220-TESTAR-FS-ADESAO

                   ADD  1              TO WRK-ACU-ATUALZ-TP-REG-2

                   ADD  1              TO ACU-GRAVADOS-ADESAO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  GRAVAR ARQUIVO REJEITA.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-GRAVAR-REJEITADOS          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COD-MSG-X           NOT EQUAL 'MSGX'
               PERFORM 3510-OBTER-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO FD-MENSAGEM-RJ
           END-IF.

           MOVE  1                     TO WRK-ERRO.

           WRITE FD-REJEITADO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 9230-TESTAR-FS-REJEITA.

           ADD  1                      TO ACU-GRAVADOS-REJEITA.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER DESCRICAO DA MENSAGEM.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3510-OBTER-MENSAGEM             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCTG2-ENTRADA,
                                       GFCTG3-SAIDA.

           MOVE SPACES                 TO GFCTG2-ENTRADA,
                                          GFCTG3-SAIDA.

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.

           MOVE 'GFCT9072'             TO GFCTG2-TRANSACAO.
           MOVE 'GFCT9072'             TO GFCTG2-FUNCAO.
           MOVE ZEROS                  TO GFCTG2-FUNC-BDSCO.
           MOVE 'B'                    TO GFCTG2-TIPO-PROC.
           MOVE WRK-COD-MSG-X          TO GFCTG2-COD-MSG.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG03-MODULO
               MOVE WRK-MSG03          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT9072'         TO GFCT0M-TRANSACAO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                                          WRK-MSG03-MODULO
                   MOVE WRK-MSG03      TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT9072'     TO GFCT0M-TRANSACAO
               END-IF

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA PROCESSAMENTO FINAL DO PROGRAMA           *
      ******************************************************************
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 9100-EMITIR-DISPLAY.

           CLOSE ENTRADA
                 ADESAO
                 REJEITA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 9200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE EMISSAO DE DISPLAY DE TOTAIS                      *
      ******************************************************************
      *----------------------------------------------------------------*
       9100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-ENTRADA           NOT EQUAL ZEROS
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '*                   PROGRAMA GFCT9072
      -        '        *'
               DISPLAY '*                   -----------------
      -        '        *'
               DISPLAY '*
      -        '        *'
               DISPLAY '*          PROCESSAMENTO  EFETUADO COM SUCESSO
      -        '        *'
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '************************************************
      -        '*********'

               MOVE ACU-LIDOS-ENTRADA      TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS LIDOS              = '
                 WRK-MASCARA
                           ' *'

               MOVE ACU-GRAVADOS-ADESAO    TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS GRAVADOS  ADESAO   = '
                   WRK-MASCARA
                            ' *'

               MOVE WRK-ACU-ATUALZ-TP-REG-1
                                           TO WRK-MASCARA
               DISPLAY '* QTDE REGISTROS ATUALZ. TIPO REG. 1    = '
                   WRK-MASCARA
                            ' *'

               MOVE WRK-ACU-ATUALZ-TP-REG-2
                                           TO WRK-MASCARA
               DISPLAY '* QTDE REGISTROS ATUALZ. TIPO REG. 2    = '
                   WRK-MASCARA
                            ' *'

               MOVE WRK-ACU-ATUALZ-TP-REG-3
                                           TO WRK-MASCARA
               DISPLAY '* QTDE REGISTROS ATUALZ. TIPO REG. 3    = '
                   WRK-MASCARA
                            ' *'

               MOVE ACU-GRAVADOS-REJEITA   TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS REJEITADOS         = '
                   WRK-MASCARA
                            ' *'

               DISPLAY '************************************************
      -        '*********'
           END-IF.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA TESTE DE FILE-STATUS.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       9200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 9210-TESTAR-FS-ENTRADA.

           PERFORM 9220-TESTAR-FS-ADESAO.

           PERFORM 9230-TESTAR-FS-REJEITA.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO ENTRADA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       9210-TESTAR-FS-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ENTRADA'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTRADA     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO ADESAO.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       9220-TESTAR-FS-ADESAO           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESAO           NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ADESAO'           TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESAO      TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO REJEITA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       9230-TESTAR-FS-REJEITA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REJEITA          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'REJEITA'          TO WRK-NOME-ARQ
               MOVE WRK-FS-REJEITA     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTE DE ERRRO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT9072'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
