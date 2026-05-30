      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1947.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *.     S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1947                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   MARIA VALERIA TORQUATO      - SONDPROC/GP.50*
      *    DATA........:   06/03/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CARGA AUTOMATICA ADESAO PACOTE POR AGENCIA/ *
      *                    CONTA PARA CLIENTES SEM PACOTE VIGENTE      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     ENTRADA                          -         *
      *                     REJEITA                          -         *
      *                     IMAG009                          -         *
      *                     ADESAO                           -         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
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
      *                   DB2PRD.V01CLIENTE_AGENCIA  CLIEV004          *
      *                   DB2PRD.V01CTA_POUPANCA     CLIEV007          *
      *                   DB2PRD.V01CTA_CORRENTE     CLIEV008          *
BI0511*                   DB2PRD.PARM_SERVC_TARIF    GFCTB0A2          *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTRK - ENTRADA E SAIDA - OBTER COD UF E MUNICIPIO DA    *
      *               AGENCIA.                                         *
      *    I#GFCTJM - ENTRADA E SAIDA - OBTER COD SEG GESTAO TARIFA.   *
      *    I#SCCE91 - AREA DE COMUNICACAO - VALIDAR SITUACAO DA CONTA. *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    POL7100C - AREA CHAMADA POOL7100 - CANCELAMENTO.            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5527 - OBTER CPF/CNPJ/POSTO DE SERVICO.                 *
      *    GFCT5534 - OBTER CODIGO SEGMENTO GESTAO TARIFA.             *
      *    GFCT5564 - OBTER COD DA UF E MUNICIPIO DA AGENCIA.          *
      *    SCCE9100 - VERIFICAR SITUACAO DA CONTA.                     *
      *    POOL0025 - SOMA DIAS NA DATA.                               *
      *    POOL1640 - SOMA DE DIAS, MESES E ANOS EM UMA DETERMINADA    *
      *               DATA COM SECULO.                                 *
      *    POOL1050 - FAZ CONEXAO COM DB2.                             *
      *    POOL7100 - AREA PARA TRATAMENTO DE ERROS.                   *
      *    POOL7600 - OBTER DATA E HORA DO SISTEMA.                    *
      *                                                                *
SP0404*----------------------------------------------------------------*
SP0404*                 U L T I M A   A L T E R A C A O                *
SP0404*----------------------------------------------------------------*
SP0404*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - SP0404       *
SP0404*----------------------------------------------------------------*
SP0404*                                                                *
SP0404*    ANALISTA....:  VALERIA TORQUATO   - SONDA/PROCWORK          *
SP0404*    DATA........:  04/04/2008                                   *
SP0404*    OBJETIVO....:  ALTERACAO NA FORMA DE IDENTIFICACAO DE       *
SP0404*                   PERMISSAO DE ADESAO A PACOTES VIGENTES NA    *
SP0404*                   TABELA GFCTB0C6 - DB2PRD.PRMSS_ADSAO_PCOTE   *
SP0404*                                                                *
      *================================================================*

231109*----------------------------------------------------------------*
231109*           SONDA PROCWORK -  CONSULTORIA - ALTERACAO            *
231109*----------------------------------------------------------------*
231109*    ANALISTA....:   MARCUS VINICIUS PINHEIRO CURTO /   GRUPO 50 *
231109*    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA    /   GRUPO 50 *
231109*    DATA........:   23/11/2009                                  *
231109*    OBJETIVO....:   INCONSISTIR AS TARIFAS 1123 E 1124 NO       *
231109*                    PROCESSO DE ADESAO.                         *
231109*----------------------------------------------------------------*
ABR10 *----------------------------------------------------------------*
ABR10 *           SONDA PROCWORK -  CONSULTORIA - ALTERACAO            *
ABR10 *----------------------------------------------------------------*
ABR10 *    ANALISTA....:   MARCUS VINICIUS PINHEIRO CURTO /   GRUPO 50 *
ABR10 *    PROGRAMADOR.:   FABRICA SOFTWARE               /   GRUPO 50 *
ABR10 *    DATA........:   ABR/2010                                    *
ABR10 *    OBJETIVO....:   INCONSISTIR AS TARIFAS CESTA CELULAR NO     *
ABR10 *                    PROCESSO DE ADESAO.                         *
ABR10 *----------------------------------------------------------------*
BI0511*                 U L T I M A   A L T E R A C A O                *
BI0511*----------------------------------------------------------------*
BI0511*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0511       *
BI0511*----------------------------------------------------------------*
BI0511*                                                                *
BI0511*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA/PROCWORK             *
BI0511*    DATA........:  05 / 2011                                    *
BI0511*    OBJETIVO....:  PROJETO CARTAO BONUS CELULAR - FASE II       *
BI0511*                                                                *
BI0412*================================================================*
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
      *----------------------------------------------------------------*
      *                 U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
      *        SONDA IT       - CONSULTORIA - ALTERACAO - BI1113       *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA....:  UBIRAJARA          - SONDA IT                *
      *    DATA........:  NOV/2013                                     *
      *    OBJETIVO....:  INCLUIR TRATAMENTO DE TIPO DE CONTA = 3      *
      *                   ( CONTA BENEFICIO ).                         *
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

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ENTRADA   ASSIGN     TO UT-S-ENTRADA
           FILE STATUS                 IS WRK-FS-ENTRADA.

           SELECT REJEITA  ASSIGN      TO UT-S-REJEITA
           FILE STATUS                 IS WRK-FS-REJEITA.

           SELECT IMAG009  ASSIGN      TO UT-S-IMAG009
           FILE STATUS                 IS WRK-FS-IMAG009.

BI0715     SELECT IMAGX09  ASSIGN      TO UT-S-IMAGX09
BI0715     FILE STATUS                 IS WRK-FS-IMAGX09.

           SELECT ADESAO   ASSIGN      TO UT-S-ADESAO
           FILE STATUS                 IS WRK-FS-ADESAO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO CONTA CORRENTE COM TARIFA GFCT.             *
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
           05  FILLER              PIC  X(005).
           05  FD-CENTRO-CUSTO     PIC  X(004).
           05  FILLER              PIC  X(034).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO RGISTROS REJEITADOS.                        *
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
           05  FILLER                  PIC  X(005).
           05  FD-CENTRO-CUSTO-RJ      PIC  X(004).
           05  FD-DATA-INIC-VIG-RJ     PIC  9(008).
           05  FD-DATA-FIM-VIG-RJ      PIC  9(008).
           05  FILLER                  PIC  X(007).
           05  FD-MENSAGEM-RJ          PIC  X(070).
           05  FILLER                  PIC  X(031).

      *----------------------------------------------------------------*
      *    OUTPUT: REGISTROS DE CARGA DA TABELA DE ADESAO.             *
      *            ORG. SEQUENCIAL     - LRECL = 97                    *
      *----------------------------------------------------------------*

       FD  IMAG009
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-IMAG009.
           05 CJUNC-DEPDC          PIC S9(005)V USAGE COMP-3.
           05 CCTA-CLI             PIC S9(007)V USAGE COMP-3.
           05 CSERVC-TARIF         PIC S9(005)V USAGE COMP-3.
           05 HINCL-REG            PIC  X(026).
           05 CFUNC-MANUT-INCL     PIC S9(009)V USAGE COMP-3.
           05 CFUNC-MANUT          PIC S9(009)V USAGE COMP-3.
           05 DINIC-ADSAO-INDVD    PIC  X(010).
           05 DFIM-ADSAO-INDVD     PIC  X(010).
           05 HMANUT-REG           PIC  X(026).
           05 CINDCD-EXCL-REG      PIC S9(001)V USAGE COMP-3.
           05 CCUSTO               PIC  X(004).

BI0715*----------------------------------------------------------------*
BI0715*    OUTPUT: REGISTROS DE CARGA DA TABELA DE ADESAO              *
BI0715*                                  SEM O CAMPO 'CENTRO DE CUSTO' *
BI0715*            ORG. SEQUENCIAL     - LRECL = 94                    *
BI0715*----------------------------------------------------------------*
BI0715
BI0715 FD  IMAGX09
BI0715     RECORDING MODE IS F
BI0715     LABEL RECORD IS STANDARD
BI0715     BLOCK CONTAINS 0 RECORDS.
BI0715
BI0715 01  FD-IMAGX09.
BI0715     05 CJUNC-DEPDC          PIC S9(005)V USAGE COMP-3.
BI0715     05 CCTA-CLI             PIC S9(007)V USAGE COMP-3.
BI0715     05 CSERVC-TARIF         PIC S9(005)V USAGE COMP-3.
BI0715     05 HINCL-REG            PIC  X(026).
BI0715     05 CFUNC-MANUT-INCL     PIC S9(009)V USAGE COMP-3.
BI0715     05 CFUNC-MANUT          PIC S9(009)V USAGE COMP-3.
BI0715     05 DINIC-ADSAO-INDVD    PIC  X(010).
BI0715     05 DFIM-ADSAO-INDVD     PIC  X(010).
BI0715     05 HMANUT-REG           PIC  X(026).
BI0715     05 CINDCD-EXCL-REG      PIC S9(001)V USAGE COMP-3.
BI0715     05 CTPO-SERVC-TARIF     PIC S9(001)V USAGE COMP-3.

      *----------------------------------------------------------------*
      *    OUTPUT: REGISTROS.                                          *
      *            ORG. SEQUENCIAL     - LRECL = 60                    *
      *----------------------------------------------------------------*

       FD  ADESAO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ADESAO.
           05 FD-AGENCIA-AD        PIC 9(005).
           05 FD-CONTA-AD          PIC 9(007).
           05 FD-TARIFA-GFCT-AD    PIC 9(005).
           05 FILLER               PIC X(005).
           05 FD-CENTRO-CUSTO-AD   PIC X(004).
           05 FD-DATA-INIC-VIG-AD  PIC 9(008).
           05 FD-DATA-FIM-VIG-AD   PIC 9(008).
           05 FILLER               PIC X(018).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*
       01  WRK-CAMPO-TIRA-SINAL        PIC +9(017)         VALUE ZEROS.
       01  FILLER  REDEFINES           WRK-CAMPO-TIRA-SINAL.
           05  WRK-CAMPO-TIRA-SINAL-S  PIC X.
           05  WRK-CAMPO-TIRA-SINAL-17 PIC 9(17).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(01).
               10 WRK-CAMPO-TIRA-SINAL-16 PIC 9(16).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(02).
               10 WRK-CAMPO-TIRA-SINAL-15 PIC 9(15).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(03).
               10 WRK-CAMPO-TIRA-SINAL-14 PIC 9(14).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(04).
               10 WRK-CAMPO-TIRA-SINAL-13 PIC 9(13).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(05).
               10 WRK-CAMPO-TIRA-SINAL-12 PIC 9(12).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(06).
               10 WRK-CAMPO-TIRA-SINAL-11 PIC 9(11).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(07).
               10 WRK-CAMPO-TIRA-SINAL-10 PIC 9(10).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(08).
               10 WRK-CAMPO-TIRA-SINAL-09 PIC 9(09).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(09).
               10 WRK-CAMPO-TIRA-SINAL-08 PIC 9(08).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(10).
               10 WRK-CAMPO-TIRA-SINAL-07 PIC 9(07).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(11).
               10 WRK-CAMPO-TIRA-SINAL-06 PIC 9(06).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(12).
               10 WRK-CAMPO-TIRA-SINAL-05 PIC 9(05).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(13).
               10 WRK-CAMPO-TIRA-SINAL-04 PIC 9(04).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(14).
               10 WRK-CAMPO-TIRA-SINAL-03 PIC 9(03).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(15).
               10 WRK-CAMPO-TIRA-SINAL-02 PIC 9(02).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(16).
               10 WRK-CAMPO-TIRA-SINAL-01 PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-01        PIC +9(01)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-01.
           05  WRK-CAMPO-POE-SINAL-01-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-01-C  PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-02        PIC +9(02)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-02.
           05  WRK-CAMPO-POE-SINAL-02-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-02-C  PIC 9(02).

       01  WRK-CAMPO-POE-SINAL-03        PIC +9(03)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-03.
           05  WRK-CAMPO-POE-SINAL-03-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-03-C  PIC 9(03).

       01  WRK-CAMPO-POE-SINAL-04        PIC +9(04)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-04.
           05  WRK-CAMPO-POE-SINAL-04-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-04-C  PIC 9(04).

       01  WRK-CAMPO-POE-SINAL-05        PIC +9(05)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-05.
           05  WRK-CAMPO-POE-SINAL-05-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-05-C  PIC 9(05).

       01  WRK-CAMPO-POE-SINAL-06        PIC +9(06)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-06.
           05  WRK-CAMPO-POE-SINAL-06-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-06-C  PIC 9(06).

       01  WRK-CAMPO-POE-SINAL-07        PIC +9(07)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-07.
           05  WRK-CAMPO-POE-SINAL-07-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-07-C  PIC 9(07).

       01  WRK-CAMPO-POE-SINAL-08        PIC +9(08)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-08.
           05  WRK-CAMPO-POE-SINAL-08-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-08-C  PIC 9(08).

       01  WRK-CAMPO-POE-SINAL-09        PIC +9(09)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-09.
           05  WRK-CAMPO-POE-SINAL-09-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-09-C  PIC 9(09).

       01  WRK-CAMPO-POE-SINAL-10        PIC +9(10)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-10.
           05  WRK-CAMPO-POE-SINAL-10-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-10-C  PIC 9(10).

       01  WRK-CAMPO-POE-SINAL-11        PIC +9(11)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-11.
           05  WRK-CAMPO-POE-SINAL-11-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-11-C  PIC 9(11).

       01  WRK-CAMPO-POE-SINAL-12        PIC +9(12)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-12.
           05  WRK-CAMPO-POE-SINAL-12-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-12-C  PIC 9(12).

       01  WRK-CAMPO-POE-SINAL-13        PIC +9(13)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-13.
           05  WRK-CAMPO-POE-SINAL-13-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-13-C  PIC 9(13).

       01  WRK-CAMPO-POE-SINAL-14        PIC +9(14)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-14.
           05  WRK-CAMPO-POE-SINAL-14-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-14-C  PIC 9(14).

       01  WRK-CAMPO-POE-SINAL-15        PIC +9(15)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-15.
           05  WRK-CAMPO-POE-SINAL-15-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-15-C  PIC 9(15).

       01  WRK-CAMPO-POE-SINAL-16        PIC +9(16)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-16.
           05  WRK-CAMPO-POE-SINAL-16-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-16-C  PIC 9(16).

       01  WRK-CAMPO-POE-SINAL-17        PIC +9(17)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-17.
           05  WRK-CAMPO-POE-SINAL-17-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-17-C  PIC 9(17).


      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(022)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ENTRADA           PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-IMAG009        PIC  9(011) COMP-3  VALUE ZEROS.
BI0715 77  ACU-GRAVADOS-IMAGX09        PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-ADESAO         PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-REJEITA        PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZADOS             PIC  9(011) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-ENTRADA              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-REJEITA              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-IMAG009              PIC  X(002)         VALUE SPACES.
BI0715 77  WRK-FS-IMAGX09              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ADESAO               PIC  X(002)         VALUE SPACES.

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
       77  WRK-IGNORA-PERMISSAO        PIC  X(001)         VALUE    'N'.
       77  WRK-IGN-PERM-CESTA1         PIC  9(005)         VALUE ZEROS.
       77  WRK-IGN-PERM-CESTA2         PIC  9(005)         VALUE ZEROS.
       77  WRK-IGN-PERM-CESTA3         PIC  9(005)         VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZZZ.ZZZ.999  VALUE ZEROS.
       77  WRK-TIPO-PESSOA-CLIENTE     PIC  X(001)         VALUE SPACES.
       77  WRK-TIPO-CONTA-CLIENTE      PIC  9(001)         VALUE ZEROS.
       77  WRK-ERRO                    PIC  X(001)         VALUE SPACES.
       77  WRK-PROX-AGRUPAMENTO        PIC  9(003)         VALUE ZEROS.
       77  WRK-SEQ-AGPTO               PIC S9(009) COMP-3  VALUE ZEROS.
       77  WRK-DT-INICIO               PIC  X(010)         VALUE SPACES.
       77  WRK-FIM-AGRUPAMENTO         PIC  X(001)         VALUE SPACES.
       77  WRK-AGRUPAMENTO-VALIDO      PIC  X(001)         VALUE SPACES.
       77  WRK-EXISTE-AGRUPAMENTO      PIC  X(001)         VALUE SPACES.
       77  WRK-MSG-PARM-VZIO           PIC  X(042)         VALUE
           'PARAMETRO NAO INFORMADO.PROGRAMA CANCELADO'.
       77  WRK-MESES-PARM-VZIO         PIC  X(050)         VALUE
           'NUMERO DE MESES NAO NUMERICO PROGRAMA CANCELADO'.
       77  WRK-MESES-PARM-ZERO         PIC  X(050)         VALUE
           'PARAMETRO CONTEM ZERO - PROGRAMA CANCELADO'.

ABR10  01  WRK-CESTA-CELULAR           PIC  X(001)         VALUE SPACES.

BI0412 01  WRK-CINDCD-ADSAO-INDVD      PIC  X(001)         VALUE 'I'.
BI0412 01  WRK-CINDCD-ADSAO-AMBAS      PIC  X(001)         VALUE 'A'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*

       01  WRK-CH-ANT.
           05  WRK-AGENCIA-ANT         PIC 9(005)          VALUE ZEROS.
           05  WRK-CONTA-ANT           PIC 9(007)          VALUE ZEROS.
           05  WRK-TARIFA-ANT          PIC 9(005)          VALUE ZEROS.

       01  WRK-CH-ATU.
           05  WRK-AGENCIA-ATU         PIC 9(005)          VALUE ZEROS.
           05  WRK-CONTA-ATU           PIC 9(007)          VALUE ZEROS.
           05  WRK-TARIFA-ATU          PIC 9(005)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA DA POOL0025'.
      *----------------------------------------------------------------*

       01  WRK-DATA                    PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-R                  REDEFINES WRK-DATA.
           05 WRK-DIA                  PIC  9(002).
           05 WRK-MES                  PIC  9(002).
           05 WRK-ANO                  PIC  9(004).

       01  WRK-DATA-ENTRADA            PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-NUMERO-MESES            PIC  9(003)         VALUE ZEROS.
       01  WRK-DATA-SAIDA              PIC  9(009) COMP-3  VALUE ZEROS.

       01  WRK-DATA-SAI-POOL           PIC  9(009)         VALUE ZEROS.
       01  WRK-DATA-SAI-POOL-R         REDEFINES WRK-DATA-SAI-POOL.
           05 FILLER                   PIC  X(001).
           05 WRK-DIA-SAI-POOL         PIC  9(002).
           05 WRK-MES-SAI-POOL         PIC  9(002).
           05 WRK-ANO-SAI-POOL         PIC  9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           'AREA PARA MENSAGENS'.
      *----------------------------------------------------------------*

       01  WRK-MSG01                   PIC  X(050)         VALUE
           'REGISTRO DUPLICADO P/ ESTA AG/CTA/TARIF'.

       01  WRK-MSG02.
           03  WRK-MSG02-01            PIC  X(040)         VALUE
           'ERRO MODULO SCCE9100 - RETURN CODE = '.
           03  WRK-MSG02-CD            PIC  X(004)         VALUE SPACES.

       01  WRK-MSG03.
           03  WRK-MSG03-01            PIC  X(041)         VALUE
           'ERRO MODULO SCCE9100 - CODIGO RETORNO = '.
           03  WRK-MSG03-CD            PIC  X(004)         VALUE SPACES.

       01  WRK-MSG04                   PIC  X(050)         VALUE
           'CLIENTE NAO ENCONTRADO NA TABELA DO CLIE'.

       01  WRK-MSG05                   PIC  X(060)         VALUE
           'TARIFA NAO ENCONTRADA NO CADASTRO DE TARIFAS - GFCTB0D8'.

       01  WRK-MSG06                   PIC  X(040)         VALUE
           'JA TEM UMA ADESAO P/ ESTA AG/CTA'.

       01  WRK-MSG07.
           03  WRK-MSG07-01            PIC  X(040)         VALUE
           'ERRO MODULO GFCT5527 - RETURN CODE = '.
           03  WRK-MSG07-CD            PIC  X(004)         VALUE SPACES.

       01  WRK-MSG08.
           03  WRK-MSG08-01            PIC  X(041)         VALUE
           'ERRO MODULO GFCT5527 - CODIGO RETORNO = '.
           03  WRK-MSG08-CD            PIC  X(004)         VALUE SPACES.

       01  WRK-MSG09.
           03  WRK-MSG09-01            PIC  X(040)         VALUE
           'ERRO MODULO GFCT5534 - RETURN CODE = '.
           03  WRK-MSG09-CD            PIC  X(004)         VALUE SPACES.

       01  WRK-MSG10.
           03  WRK-MSG10-01            PIC  X(041)         VALUE
           'ERRO MODULO GFCT5534 - CODIGO RETORNO = '.
           03  WRK-MSG10-CD            PIC  X(004)         VALUE SPACES.

       01  WRK-MSG11                   PIC  X(041)         VALUE
           'AGENCIA/CONTA/TARIFA SEM AGRUPAMENTO    '.

       01  WRK-MSG12                   PIC  X(041)         VALUE
           'CTPO-CTA-DEB-TARIF TABELA DIFERENTE CLIE'.

       01  WRK-MSG13.
           03  WRK-MSG13-01            PIC  X(040)         VALUE
           'ERRO MODULO GFCT5564 - RETURN CODE = '.
           03  WRK-MSG13-CD            PIC  X(004)         VALUE SPACES.

       01  WRK-MSG14.
           03  WRK-MSG14-01            PIC  X(041)         VALUE
           'ERRO MODULO GFCT5564 - CODIGO RETORNO = '.
           03  WRK-MSG14-CD            PIC  X(004)         VALUE SPACES.

       01  WRK-MSG15                   PIC  X(041)         VALUE
           'ERRO NA POOL0025 - DADOS INCONSISTENTES'.

       01  WRK-MSG16                   PIC  X(040)         VALUE
           'TARIFA PACOTE NAO EXISTE TAB. GFCTB0A2'.

       01  WRK-MSG17                   PIC  X(040)         VALUE
           'DT INICIO GERADA MENOR OU IGUAL DT TAB'.

SP0404 01  WRK-MSG18                   PIC  X(041)         VALUE
SP0404     'CPSSOA-SERVC-TARIF TABELA DIFERENTE CLIE'.

231109 01  WRK-MSG2311                 PIC  X(041)         VALUE
231109     'TARIFAS CESTAS CONTA CELULAR NAO PERMIT.'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(015)         VALUE
           'AREA PARA DATAS'.
      *----------------------------------------------------------------*

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

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

       01  WRK-DATA-DB2                PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-DB2-R              REDEFINES WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002).
           05  WRK-PTO1                PIC  X(001).
           05  WRK-MES-DB2             PIC  9(002).
           05  WRK-PTO2                PIC  X(001).
           05  WRK-ANO-DB2             PIC  9(004).

       01  WRK-DATA-INIC-ADESAO        PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-INI-R              REDEFINES WRK-DATA-INIC-ADESAO.
           05  WRK-DIA-INI             PIC  9(002).
           05  WRK-PTO3                PIC  X(001).
           05  WRK-MES-INI             PIC  9(002).
           05  WRK-PTO4                PIC  X(001).
           05  WRK-ANO-INI             PIC  9(004).

       01  WRK-DTINIC-INV              PIC  X(008)         VALUE SPACES.
       01  WRK-DTINIC-INV-R            REDEFINES WRK-DTINIC-INV.
           05  WRK-DTINIC-INV-ANO      PIC  9(004).
           05  WRK-DTINIC-INV-MES      PIC  9(002).
           05  WRK-DTINIC-INV-DIA      PIC  9(002).

       01  WRK-DTFIM-INV               PIC  X(008)         VALUE SPACES.
       01  WRK-DTFIM-INV-R             REDEFINES WRK-DTFIM-INV.
           05  WRK-DTFIM-INV-ANO       PIC  9(004).
           05  WRK-DTFIM-INV-MES       PIC  9(002).
           05  WRK-DTFIM-INV-DIA       PIC  9(002).

       01  WRK-DTINIC-TAB-INV          PIC  X(008)         VALUE SPACES.
       01  WRK-DTINIC-TAB-INV-R        REDEFINES WRK-DTINIC-TAB-INV.
           05  WRK-DTINIC-TAB-INV-ANO  PIC  9(004).
           05  WRK-DTINIC-TAB-INV-MES  PIC  9(002).
           05  WRK-DTINIC-TAB-INV-DIA  PIC  9(002).

       01  WRK-DTFIM-TAB-INV          PIC  X(008)         VALUE SPACES.
       01  WRK-DTFIM-TAB-INV-R        REDEFINES WRK-DTFIM-TAB-INV.
           05  WRK-DTFIM-TAB-INV-ANO  PIC  9(004).
           05  WRK-DTFIM-TAB-INV-MES  PIC  9(002).
           05  WRK-DTFIM-TAB-INV-DIA  PIC  9(002).

       01  WRK-DINIC-ADSAO-DB2        PIC  X(010)         VALUE SPACES.
       01  WRK-DINIC-ADSAO-DB2-R      REDEFINES WRK-DINIC-ADSAO-DB2.
           05  WRK-DINIC-ADSAO-DIA    PIC  9(002).
           05  WRK-DINIC-ADSAO-PT1    PIC  X(001).
           05  WRK-DINIC-ADSAO-MES    PIC  9(002).
           05  WRK-DINIC-ADSAO-PT2    PIC  X(001).
           05  WRK-DINIC-ADSAO-ANO    PIC  9(004).

       01  WRK-DDFIM-ADSAO-DB2        PIC  X(010)         VALUE SPACES.
       01  WRK-DDFIM-ADSAO-DB2-R      REDEFINES WRK-DDFIM-ADSAO-DB2.
           05  WRK-DDFIM-ADSAO-DIA    PIC  9(002).
           05  WRK-DDFIM-ADSAO-PT1    PIC  X(001).
           05  WRK-DDFIM-ADSAO-MES    PIC  9(002).
           05  WRK-DDFIM-ADSAO-PT2    PIC  X(001).
           05  WRK-DDFIM-ADSAO-ANO    PIC  9(004).

       01  WRK-DATA-FIM-ADESAO         PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-FIM-R              REDEFINES WRK-DATA-FIM-ADESAO.
           05  WRK-DIA-FIM             PIC  9(002).
           05  WRK-PTO5                PIC  X(001).
           05  WRK-MES-FIM             PIC  9(002).
           05  WRK-PTO6                PIC  X(001).
           05  WRK-ANO-FIM             PIC  9(004).

       01  WRK-DATA-INIC-AD            PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INIC-AD-R          REDEFINES WRK-DATA-INIC-AD.
           05  WRK-DIA-INI-AD          PIC  9(002).
           05  WRK-MES-INI-AD          PIC  9(002).
           05  WRK-ANO-INI-AD          PIC  9(004).

       01  WRK-DATA-FIM-AD             PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-FIM-AD-R          REDEFINES WRK-DATA-FIM-AD.
           05  WRK-DIA-FIM-AD          PIC  9(002).
           05  WRK-MES-FIM-AD          PIC  9(002).
           05  WRK-ANO-FIM-AD          PIC  9(004).

       01  WRK-DPROCM-INV              PIC  9(008)         VALUE ZEROS.
       01  WRK-DINIC-INV               PIC  9(008)         VALUE ZEROS.

       01  WRK-TIMESTAMP-AUX.
           05  WRK-TMP-ANO             PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-TMP-MES             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-TMP-DIA             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-TMP-HOR             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-TMP-MIN             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-TMP-SEG             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-TMP-MIL             PIC  9(006)         VALUE ZEROS.

       01  WRK-TIMESTAMP-AUX-R         REDEFINES WRK-TIMESTAMP-AUX
                                       PIC  X(026).

       01  WRK-DINIC-VGCIA-TARIF       PIC  X(010)         VALUE SPACES.
       01  WRK-DFIM-VGCIA-TARIF        PIC  X(010)         VALUE SPACES.

       01  WRK-DATA-FIM-TESTE.
           05  WRK-ANO-FIMT             PIC  9(004)        VALUE ZEROS.
           05  WRK-MES-FIMT             PIC  9(002)        VALUE ZEROS.
           05  WRK-DIA-FIMT             PIC  9(002)        VALUE ZEROS.

       01  WRK-DATA-INIC-TESTE.
           05  WRK-ANO-INIT             PIC  9(004)        VALUE ZEROS.
           05  WRK-MES-INIT             PIC  9(002)        VALUE ZEROS.
           05  WRK-DIA-INIT             PIC  9(002)        VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(018)         VALUE
           'AREA PARA POOL1640'.
      *----------------------------------------------------------------*

       01  1640-CAMPOS.
           02  1640-DATA-ENTRADA    PIC X(10)  VALUE SPACES.
           02  1640-FORMATO-ENTRADA PIC 9(2)   VALUE ZEROS.
           02  1640-TIPO-INCREMENTO PIC X(2)   VALUE SPACES.
           02  1640-INCREMENTO      PIC 9(3)   VALUE ZEROS.
           02  1640-FORMATO-SAIDA   PIC 9(02)  VALUE ZEROS.
           02  1640-DATA-SAIDA      PIC X(10)  VALUE SPACES.
           02  1640-MENSAGEM        PIC X(80)  VALUE SPACES.

       01  WRK-DATA-DB2-1640.
           05  WRK-DIA-DB2-1640        PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-MES-DB2-1640        PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-ANO-DB2-1640        PIC  9(004) VALUE ZEROS.

       01  WRK-NUMERO-DIAS             PIC  S9(005) COMP-3 VALUE ZEROS.

       01  WRK-DATA-SAIDA1640.
           05  WRK-DIA-SAIDA1640       PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-MES-SAIDA1640       PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-ANO-SAIDA1640       PIC  9(004) VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(026)         VALUE
           'AREA DE ERRO - FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
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
           'AREA DA POOL7600'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-TIMESTAMP.
             10  WRK-ANO-TM            PIC  9(004)         VALUE ZEROS.
             10  WRK-MES-TM            PIC  9(002)         VALUE ZEROS.
             10  WRK-DIA-TM            PIC  9(002)         VALUE ZEROS.
             10  WRK-HOR               PIC  9(002)         VALUE ZEROS.
             10  WRK-MIN               PIC  9(002)         VALUE ZEROS.
             10  WRK-SEG               PIC  9(002)         VALUE ZEROS.
             10  WRK-MIL               PIC  X(006)         VALUE SPACES.

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
           03  WRK-IO-TERM              PIC  X(008)     VALUE SPACES.
           03  FILLER                   PIC  X(002)     VALUE SPACES.
           03  WRK-IO-STA               PIC  X(002)     VALUE SPACES.
           03  FILLER                   PIC  X(012)     VALUE SPACES.
           03  WRK-IO-MODNAME           PIC  X(008)     VALUE SPACES.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME          PIC  X(008)     VALUE SPACES.
           03  FILLER                   PIC  X(002)     VALUE SPACES.
           03  WRK-ALT-STATUS           PIC  X(002)     VALUE SPACES.
           03  FILLER                   PIC  X(012)     VALUE SPACES.
           03  WRK-ALT-MODNAME          PIC  X(008)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM SCCE9100 *'.
      *----------------------------------------------------------------*

       COPY 'I#SCCE91'.

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
           ' AREA PARA POOL7100'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA PARA ERROS'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT0M'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB009
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
             INCLUDE GFCTB0A2
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-AREA-PARM.
           05 LNK-TAMA-PARM               PIC S9(004) COMP.
           05 LNK-QTDE-MESES              PIC  9(003).
           05 FILLER                      PIC  X(001).
           05 LNK-PERMISSAO.
              10 LNK-IGNORA-PERMISSAO    PIC  X(001).
              10 FILLER                  PIC  X(001).
              10 LNK-IGNORA-PERM-CESTAS.
                 20 LNK-IGN-PERM-CESTA1.
                    25 LNK-IGN-PERM-CESTA1-R PIC 9(005).
                 20 FILLER            PIC  X(001).
                 20 LNK-IGN-PERM-CESTA2.
                    25 LNK-IGN-PERM-CESTA2-R PIC 9(005).
                 20 FILLER            PIC  X(001).
                 20 LNK-IGN-PERM-CESTA3.
                    25 LNK-IGN-PERM-CESTA3-R PIC 9(005).


      *================================================================*
       PROCEDURE                       DIVISION USING LNK-AREA-PARM.
      *================================================================*

           MOVE '+' TO
               WRK-CAMPO-TIRA-SINAL-S
               WRK-CAMPO-POE-SINAL-01-S
               WRK-CAMPO-POE-SINAL-02-S
               WRK-CAMPO-POE-SINAL-03-S
               WRK-CAMPO-POE-SINAL-04-S
               WRK-CAMPO-POE-SINAL-05-S
               WRK-CAMPO-POE-SINAL-06-S
               WRK-CAMPO-POE-SINAL-07-S
               WRK-CAMPO-POE-SINAL-08-S
               WRK-CAMPO-POE-SINAL-09-S
               WRK-CAMPO-POE-SINAL-10-S
               WRK-CAMPO-POE-SINAL-11-S
               WRK-CAMPO-POE-SINAL-12-S
               WRK-CAMPO-POE-SINAL-13-S
               WRK-CAMPO-POE-SINAL-14-S
               WRK-CAMPO-POE-SINAL-15-S
               WRK-CAMPO-POE-SINAL-16-S
               WRK-CAMPO-POE-SINAL-17-S.

           MOVE ZEROS TO
               WRK-CAMPO-TIRA-SINAL
               WRK-CAMPO-TIRA-SINAL-17
               WRK-CAMPO-TIRA-SINAL-16
               WRK-CAMPO-TIRA-SINAL-15
               WRK-CAMPO-TIRA-SINAL-14
               WRK-CAMPO-TIRA-SINAL-13
               WRK-CAMPO-TIRA-SINAL-12
               WRK-CAMPO-TIRA-SINAL-11
               WRK-CAMPO-TIRA-SINAL-10
               WRK-CAMPO-TIRA-SINAL-09
               WRK-CAMPO-TIRA-SINAL-08
               WRK-CAMPO-TIRA-SINAL-07
               WRK-CAMPO-TIRA-SINAL-06
               WRK-CAMPO-TIRA-SINAL-05
               WRK-CAMPO-TIRA-SINAL-04
               WRK-CAMPO-TIRA-SINAL-03
               WRK-CAMPO-TIRA-SINAL-02
               WRK-CAMPO-TIRA-SINAL-01
               WRK-CAMPO-POE-SINAL-01
               WRK-CAMPO-POE-SINAL-01-C
               WRK-CAMPO-POE-SINAL-02
               WRK-CAMPO-POE-SINAL-02-C
               WRK-CAMPO-POE-SINAL-03
               WRK-CAMPO-POE-SINAL-03-C
               WRK-CAMPO-POE-SINAL-04
               WRK-CAMPO-POE-SINAL-04-C
               WRK-CAMPO-POE-SINAL-05
               WRK-CAMPO-POE-SINAL-05-C
               WRK-CAMPO-POE-SINAL-06
               WRK-CAMPO-POE-SINAL-06-C
               WRK-CAMPO-POE-SINAL-07
               WRK-CAMPO-POE-SINAL-07-C
               WRK-CAMPO-POE-SINAL-08
               WRK-CAMPO-POE-SINAL-08-C
               WRK-CAMPO-POE-SINAL-09
               WRK-CAMPO-POE-SINAL-09-C
               WRK-CAMPO-POE-SINAL-10
               WRK-CAMPO-POE-SINAL-10-C
               WRK-CAMPO-POE-SINAL-11
               WRK-CAMPO-POE-SINAL-11-C
               WRK-CAMPO-POE-SINAL-12
               WRK-CAMPO-POE-SINAL-12-C
               WRK-CAMPO-POE-SINAL-13
               WRK-CAMPO-POE-SINAL-13-C
               WRK-CAMPO-POE-SINAL-14
               WRK-CAMPO-POE-SINAL-14-C
               WRK-CAMPO-POE-SINAL-15
               WRK-CAMPO-POE-SINAL-15-C
               WRK-CAMPO-POE-SINAL-16
               WRK-CAMPO-POE-SINAL-16-C
               WRK-CAMPO-POE-SINAL-17
               WRK-CAMPO-POE-SINAL-17-C.

           IF LNK-IGNORA-PERMISSAO EQUAL 'S' OR 'N'
              MOVE LNK-IGNORA-PERMISSAO TO
                   WRK-IGNORA-PERMISSAO
           ELSE
              MOVE 'N'                  TO
                   WRK-IGNORA-PERMISSAO
           END-IF.

           MOVE ZEROS TO                WRK-IGN-PERM-CESTA1
                                        WRK-IGN-PERM-CESTA2
                                        WRK-IGN-PERM-CESTA3.

           IF WRK-IGNORA-PERMISSAO EQUAL 'S'
              IF LNK-IGN-PERM-CESTA1 NUMERIC AND
                 LNK-IGN-PERM-CESTA1-R NOT EQUAL ZEROS
                 MOVE LNK-IGN-PERM-CESTA1-R TO
                      WRK-IGN-PERM-CESTA1
              END-IF
              IF LNK-IGN-PERM-CESTA2 NUMERIC AND
                 LNK-IGN-PERM-CESTA2-R NOT EQUAL ZEROS
                 MOVE LNK-IGN-PERM-CESTA2-R TO
                      WRK-IGN-PERM-CESTA2
              END-IF
              IF LNK-IGN-PERM-CESTA3 NUMERIC AND
                 LNK-IGN-PERM-CESTA3-R NOT EQUAL ZEROS
                 MOVE LNK-IGN-PERM-CESTA3-R TO
                      WRK-IGN-PERM-CESTA3
              END-IF
           END-IF.

      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-FS-ENTRADA          EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  ENTRADA
                OUTPUT REJEITA
                       ADESAO
                       IMAG009
BI0715                 IMAGX09.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1140-CONSISTIR-PARAMETRO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA TESTE DE FILE-STATUS.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ENTRADA.

           PERFORM 1120-TESTAR-FS-REJEITA.

           PERFORM 1130-TESTAR-FS-IMAG009.

BI0715     PERFORM 1132-TESTAR-FS-IMAGX09.

           PERFORM 1135-TESTAR-FS-ADESAO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO ENTRADA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ENTRADA'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTRADA     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO REJEITA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-REJEITA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REJEITA          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'REJEITA'          TO WRK-NOME-ARQ
               MOVE WRK-FS-REJEITA     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO IMAG009.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-IMAG009          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-IMAG009          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'IMAG009'          TO WRK-NOME-ARQ
               MOVE WRK-FS-IMAG009     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BI0715*----------------------------------------------------------------*
BI0715 1132-TESTAR-FS-IMAGX09          SECTION.
BI0715*----------------------------------------------------------------*
BI0715
BI0715     IF  WRK-FS-IMAGX09          NOT EQUAL '00'
BI0715         MOVE 'APL'              TO ERR-TIPO-ACESSO
BI0715         MOVE 'IMAGX09'          TO WRK-NOME-ARQ
BI0715         MOVE WRK-FS-IMAGX09     TO WRK-FILE-STATUS
BI0715         MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
BI0715         PERFORM 9999-PROCESSAR-ROTINA-ERRO
BI0715     END-IF.
BI0715
BI0715*----------------------------------------------------------------*
BI0715 1132-99-FIM.                    EXIT.
BI0715*----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO ADESAO.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1135-TESTAR-FS-ADESAO          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESAO           NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ADESAO'           TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESAO      TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1135-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSISTENCIA DOS CAMPOS                                     *
      *----------------------------------------------------------------*
       1140-CONSISTIR-PARAMETRO        SECTION.
      *----------------------------------------------------------------*

           IF  LNK-TAMA-PARM           EQUAL ZEROS
               DISPLAY '********** GFCT1947 **********'
               DISPLAY '*                            *'
               DISPLAY '*  PARAMETRO NAO INFORMADO   *'
               DISPLAY '*  PROGRAMA CANCELADO        *'
               DISPLAY '*                            *'
               DISPLAY '********** GFCT1947 **********'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-PARM-VZIO  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  LNK-QTDE-MESES           NOT NUMERIC
               DISPLAY '********** GFCT1947 **********'
               DISPLAY '*                            *'
               DISPLAY '*  MESES NAO NUMERICOS       *'
               DISPLAY '*  PROGRAMA CANCELADO        *'
               DISPLAY '*                            *'
               DISPLAY '********** GFCT1947 **********'
               MOVE 'APL'                TO ERR-TIPO-ACESSO
               MOVE WRK-MESES-PARM-VZIO  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  LNK-QTDE-MESES           EQUAL ZEROS
               DISPLAY '********** GFCT1947 **********'
               DISPLAY '*                            *'
               DISPLAY '*  PARAMETRO ZERADO          *'
               DISPLAY '*  PROGRAMA CANCELADO        *'
               DISPLAY '*                            *'
               DISPLAY '********** GFCT1947 **********'
               MOVE 'APL'                TO ERR-TIPO-ACESSO
               MOVE WRK-MESES-PARM-ZERO  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    VERIFICAR SE ARQUIVO ENTRADA ESTA VAZIO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ENTRADA.

           IF  WRK-FS-ENTRADA          NOT EQUAL '10'
               PERFORM 2200-ACESSAR-DPROCM
           END-IF.

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

           IF  WRK-FS-ENTRADA              EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA                TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ENTRADA.

           ADD 1                           TO ACU-LIDOS-ENTRADA.

           MOVE FD-AGENCIA                 TO WRK-AGENCIA-ATU.
           MOVE FD-CONTA                   TO WRK-CONTA-ATU.
           MOVE FD-TARIFA-GFCT             TO WRK-TARIFA-ATU.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE LE TABELA GFCTBA1 E BUSCA DATA PROCM             *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ACESSAR-DPROCM             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_PROX,
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-PROX,
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'PARM_DATA_PROCM' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL      OF GFCTB0A1 TO WRK-DATA-DB2
           MOVE WRK-DIA-DB2                   TO WRK-DIA-INV
           MOVE WRK-MES-DB2                   TO WRK-MES-INV
           MOVE WRK-ANO-DB2                   TO WRK-ANO-INV
           MOVE '.'                           TO WRK-PTO1
                                                 WRK-PTO2.
           MOVE WRK-DATA-INV                  TO WRK-DPROCM-INV
           MOVE 01                            TO WRK-DIA-DB2

           IF WRK-MES-DB2 EQUAL 12
               MOVE 01 TO WRK-MES-DB2
               ADD   1 TO WRK-ANO-DB2
           ELSE
               ADD 1   TO WRK-MES-DB2
           END-IF.

           MOVE WRK-DATA-DB2                  TO WRK-DATA-INIC-ADESAO.

           MOVE WRK-DIA-INI                   TO WRK-DTINIC-INV-DIA.
           MOVE WRK-MES-INI                   TO WRK-DTINIC-INV-MES.
           MOVE WRK-ANO-INI                   TO WRK-DTINIC-INV-ANO.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSA POOL1640 E CALCULA MESES SOMANDO A DATA FIM DE VIGENCIA *
      * ACESSA POOL0025 E CALCULA DATA FIM OBTENDO O ULTIMO DIA DO MES *
      ******************************************************************
      *----------------------------------------------------------------*
       2205-CALC-DT-FIM                SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-PROX            OF GFCTB0A1
                                       TO WRK-DATA-DB2-1640.
           MOVE LNK-QTDE-MESES         TO WRK-NUMERO-MESES

           MOVE WRK-DATA-DB2-1640     TO 1640-DATA-ENTRADA.
           MOVE 09                    TO 1640-FORMATO-ENTRADA
                                         1640-FORMATO-SAIDA.
           MOVE WRK-NUMERO-MESES      TO 1640-INCREMENTO.
           MOVE 'M'                   TO 1640-TIPO-INCREMENTO.


           CALL 'POOL1640'             USING 1640-DATA-ENTRADA
                                             1640-FORMATO-ENTRADA
                                             1640-TIPO-INCREMENTO
                                             1640-INCREMENTO
                                             1640-FORMATO-SAIDA
                                             1640-DATA-SAIDA
                                             1640-MENSAGEM.

           IF RETURN-CODE              NOT EQUAL ZEROS
              DISPLAY '**** GFCT1947 - BRAD 1640 ****'
              DISPLAY '*                            *'
              DISPLAY '* ERRO NA BRAD               *'
              DISPLAY '* '1640-MENSAGEM
              DISPLAY '*                            *'
              DISPLAY '**** GFCT1947 - BRAD 1640 ****'
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               MOVE 1640-DATA-SAIDA        TO WRK-DATA-SAIDA1640
               MOVE WRK-DIA-SAIDA1640      TO WRK-DIA-FIM
               MOVE WRK-MES-SAIDA1640      TO WRK-MES-FIM
               MOVE WRK-ANO-SAIDA1640      TO WRK-ANO-FIM
           END-IF.

      ********** ALTERACAO DE DATA FIM DE ADESAO  **********

           MOVE 01 TO WRK-DIA-FIM

           IF WRK-MES-FIM EQUAL 12
               ADD  1  TO WRK-ANO-FIM
               MOVE 01 TO WRK-MES-FIM
           ELSE
               ADD 1 TO WRK-MES-FIM
           END-IF.

           MOVE WRK-DIA-FIM            TO WRK-DIA.
           MOVE WRK-MES-FIM            TO WRK-MES.
           MOVE WRK-ANO-FIM            TO WRK-ANO.
           MOVE WRK-DATA               TO WRK-DATA-ENTRADA.
           MOVE -01                    TO WRK-NUMERO-DIAS

           CALL 'POOL0025'             USING WRK-DATA-ENTRADA
                                             WRK-NUMERO-DIAS
                                             WRK-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MSG15          TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DATA-SAIDA         TO WRK-DATA-SAI-POOL.
           MOVE '.'                    TO WRK-PTO5
                                          WRK-PTO6

           MOVE WRK-ANO-SAI-POOL       TO WRK-ANO-FIM.

           MOVE WRK-MES-SAI-POOL       TO WRK-MES-FIM.

           MOVE WRK-DIA-SAI-POOL       TO WRK-DIA-FIM.

      ********** ALTERACAO DE DATA FIM DE ADESAO  **********

           MOVE WRK-DIA-FIM TO WRK-DIA-FIMT.
           MOVE WRK-MES-FIM TO WRK-MES-FIMT.
           MOVE WRK-ANO-FIM TO WRK-ANO-FIMT.

           MOVE WRK-DIA-INI TO WRK-DIA-INIT.
           MOVE WRK-MES-INI TO WRK-MES-INIT.
           MOVE WRK-ANO-INI TO WRK-ANO-INIT.

           IF WRK-DATA-FIM-TESTE LESS WRK-DATA-INIC-TESTE
               MOVE 01 TO WRK-DIA-FIM
               MOVE WRK-MES-INI TO WRK-MES-FIM

               IF WRK-MES-FIM EQUAL 12
                   ADD  1  TO WRK-ANO-FIM
                   MOVE 01 TO WRK-MES-FIM
               ELSE
                   ADD 1 TO WRK-MES-FIM
               END-IF

               MOVE WRK-DIA-FIM            TO WRK-DIA
               MOVE WRK-MES-FIM            TO WRK-MES
               MOVE WRK-ANO-FIM            TO WRK-ANO
               MOVE WRK-DATA               TO WRK-DATA-ENTRADA
               MOVE -01                    TO WRK-NUMERO-DIAS

               CALL 'POOL0025'             USING WRK-DATA-ENTRADA
                                                 WRK-NUMERO-DIAS
                                                 WRK-DATA-SAIDA

               IF  RETURN-CODE             NOT EQUAL ZEROS
                   MOVE WRK-MSG15          TO ERR-TEXTO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF

               MOVE WRK-DATA-SAIDA         TO WRK-DATA-SAI-POOL
               MOVE '.'                    TO WRK-PTO5
                                              WRK-PTO6

               MOVE WRK-ANO-SAI-POOL       TO WRK-ANO-FIM
               MOVE WRK-MES-SAI-POOL       TO WRK-MES-FIM
               MOVE WRK-DIA-SAI-POOL       TO WRK-DIA-FIM
           END-IF.
           MOVE WRK-ANO-FIM TO WRK-DTFIM-INV-ANO.
           MOVE WRK-MES-FIM TO WRK-DTFIM-INV-MES.
           MOVE WRK-DIA-FIM TO WRK-DTFIM-INV-DIA.

      *----------------------------------------------------------------*
       2205-99-FIM.                    EXIT.
      *----------------------------------------------------------------

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS              TO WRK-ERRO.

           IF  WRK-CH-ATU          EQUAL WRK-CH-ANT
               MOVE FD-ENTRADA     TO FD-REJEITADO
               MOVE WRK-MSG01      TO FD-MENSAGEM-RJ
               PERFORM 3100-GRAVAR-REJEITADOS
           ELSE
               MOVE WRK-CH-ATU     TO WRK-CH-ANT

231109*********IF FD-TARIFA-GFCT EQUAL 01123 OR 01124
231109*************MOVE FD-ENTRADA     TO FD-REJEITADO
231109*************MOVE WRK-MSG2311    TO FD-MENSAGEM-RJ
231109*************PERFORM 3100-GRAVAR-REJEITADOS
231109*********ELSE
                   PERFORM 3150-VER-TARIFA-PACOTE-CAD
                   IF WRK-ERRO         EQUAL ZEROS
                      IF WRK-CESTA-CELULAR = 'N'
                         PERFORM 3170-GRAVAR-IMAG009
                      ELSE
                         MOVE FD-ENTRADA     TO FD-REJEITADO
                         MOVE WRK-MSG2311    TO FD-MENSAGEM-RJ
                         PERFORM 3100-GRAVAR-REJEITADOS
                      END-IF
                   END-IF
231109*********END-IF
           END-IF.

           PERFORM 2100-LER-ENTRADA.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  GRAVAR ARQUIVO REJEITA.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-GRAVAR-REJEITADOS          SECTION.
      *----------------------------------------------------------------*

           MOVE  1                     TO WRK-ERRO.

           WRITE FD-REJEITADO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-REJEITA.

           ADD 1                       TO ACU-GRAVADOS-REJEITA.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR A EXISTENCIA DE TARIFA PACOTE JA CADASTRADA.         *
      ******************************************************************
      *----------------------------------------------------------------*
       3150-VER-TARIFA-PACOTE-CAD      SECTION.
      *----------------------------------------------------------------*

           MOVE FD-AGENCIA             TO CJUNC-DEPDC       OF GFCTB009.
           MOVE FD-CONTA               TO CCTA-CLI          OF GFCTB009.
           MOVE WRK-DATA-INIC-ADESAO   TO DFIM-ADSAO-INDVD  OF GFCTB009.
           MOVE 0                      TO CINDCD-EXCL-REG   OF GFCTB009.

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

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'               TO ERR-TIPO-ACESSO
                MOVE 'ADSAO_INDVD_PCOTE' TO ERR-DBD-TAB
                MOVE 'SELECT'            TO ERR-FUN-COMANDO
                MOVE  SQLCODE            TO ERR-SQL-CODE
                MOVE '0020'              TO ERR-LOCAL
                MOVE  SPACES             TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                   EQUAL ZEROS OR -811
               MOVE FD-ENTRADA           TO FD-REJEITADO
               MOVE WRK-MSG06            TO FD-MENSAGEM-RJ
               PERFORM 3100-GRAVAR-REJEITADOS
           ELSE
               PERFORM 3160-OBTER-DADOS-GFCTB0A2
           END-IF.

      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER VIGENCIA DA TARIFA PACOTE                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3160-OBTER-DADOS-GFCTB0A2       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-INIC-ADESAO   TO WRK-DINIC-VGCIA-TARIF
                                          WRK-DFIM-VGCIA-TARIF.
           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
             SELECT
                   DINIC_VGCIA_TARIF,
                   DFIM_VGCIA_TARIF,
                   CINDCD_CREDT_CLULR
             INTO
                   :GFCTB0A2.DINIC-VGCIA-TARIF,
                   :GFCTB0A2.DFIM-VGCIA-TARIF,
                   :GFCTB0A2.CINDCD-CREDT-CLULR
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF         = :GFCTB0A2.CSERVC-TARIF     AND
                  (DINIC_VGCIA_TARIF   <= :WRK-DINIC-VGCIA-TARIF     AND
                   DFIM_VGCIA_TARIF    >  :WRK-DFIM-VGCIA-TARIF)
IN           ORDER BY DINIC_VGCIA_TARIF DESC
IN           FETCH FIRST 1 ROWS ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_SERVC_TARIF' TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL ZEROS
               IF CINDCD-CREDT-CLULR OF GFCTB0A2 = 'S'
                  MOVE 'S' TO WRK-CESTA-CELULAR
               ELSE
                  MOVE 'N' TO WRK-CESTA-CELULAR
               END-IF
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE FD-ENTRADA           TO FD-REJEITADO
               MOVE WRK-MSG16            TO FD-MENSAGEM-RJ
               PERFORM 3100-GRAVAR-REJEITADOS
           ELSE
               PERFORM 2205-CALC-DT-FIM
               MOVE DINIC-VGCIA-TARIF   OF GFCTB0A2
                                        TO WRK-DINIC-ADSAO-DB2
               MOVE WRK-DINIC-ADSAO-DIA TO WRK-DTINIC-TAB-INV-DIA
               MOVE WRK-DINIC-ADSAO-MES TO WRK-DTINIC-TAB-INV-MES
               MOVE WRK-DINIC-ADSAO-ANO TO WRK-DTINIC-TAB-INV-ANO

               MOVE DFIM-VGCIA-TARIF    OF GFCTB0A2
                                        TO WRK-DDFIM-ADSAO-DB2
               MOVE WRK-DDFIM-ADSAO-DIA TO WRK-DTFIM-TAB-INV-DIA
               MOVE WRK-DDFIM-ADSAO-MES TO WRK-DTFIM-TAB-INV-MES
               MOVE WRK-DDFIM-ADSAO-ANO TO WRK-DTFIM-TAB-INV-ANO

               IF WRK-DTINIC-INV NOT GREATER WRK-DTINIC-TAB-INV
                   MOVE FD-ENTRADA           TO FD-REJEITADO
                   MOVE WRK-MSG17            TO FD-MENSAGEM-RJ
                   PERFORM 3100-GRAVAR-REJEITADOS
               ELSE
                   IF WRK-DTFIM-INV NOT LESS WRK-DTFIM-TAB-INV

                       MOVE WRK-DTFIM-TAB-INV TO WRK-DTFIM-INV

                       MOVE WRK-DTFIM-INV-ANO TO WRK-ANO-FIM
                       MOVE WRK-DTFIM-INV-MES TO WRK-MES-FIM
                       MOVE WRK-DTFIM-INV-DIA TO WRK-DIA-FIM
                       MOVE '.'               TO WRK-PTO5
                                                 WRK-PTO6

                       PERFORM 3200-CONSISTE-AG-CTA
                   ELSE
                       PERFORM 3200-CONSISTE-AG-CTA
                   END-IF
               END-IF
           END-IF.


      *----------------------------------------------------------------*
       3160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  GRAVAR ARQUIVO IMAG009                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3170-GRAVAR-IMAG009             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FD-IMAG009.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-ANO-TM             TO WRK-TMP-ANO.
           MOVE WRK-MES-TM             TO WRK-TMP-MES.
           MOVE WRK-DIA-TM             TO WRK-TMP-DIA.
           MOVE WRK-HOR                TO WRK-TMP-HOR.
           MOVE WRK-MIN                TO WRK-TMP-MIN.
           MOVE WRK-SEG                TO WRK-TMP-SEG.
           MOVE WRK-MIL                TO WRK-TMP-MIL.
           MOVE WRK-TIMESTAMP-AUX      TO HINCL-REG OF FD-IMAG009.

           MOVE FD-AGENCIA             TO CJUNC-DEPDC      OF FD-IMAG009
           MOVE FD-CONTA               TO CCTA-CLI         OF FD-IMAG009
           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF     OF FD-IMAG009
           MOVE FD-CENTRO-CUSTO        TO CCUSTO           OF FD-IMAG009
           MOVE 1947                   TO CFUNC-MANUT-INCL OF FD-IMAG009
           MOVE ZEROS                  TO CFUNC-MANUT      OF FD-IMAG009
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-ADSAO-INDVD
                                                           OF FD-IMAG009
           MOVE '.'                           TO WRK-PTO5
                                                 WRK-PTO6.

           MOVE WRK-DATA-FIM-ADESAO    TO DFIM-ADSAO-INDVD OF FD-IMAG009
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG       OF FD-IMAG009
           MOVE ZEROS                  TO CINDCD-EXCL-REG  OF FD-IMAG009

           WRITE FD-IMAG009.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-IMAG009.

           ADD 1                       TO ACU-GRAVADOS-IMAG009.

BI0715     INITIALIZE FD-IMAGX09.
BI0715
BI0715     CALL 'POOL7600'             USING WRK-DATA-HORA.
BI0715
BI0715     MOVE WRK-ANO-TM             TO WRK-TMP-ANO.
BI0715     MOVE WRK-MES-TM             TO WRK-TMP-MES.
BI0715     MOVE WRK-DIA-TM             TO WRK-TMP-DIA.
BI0715     MOVE WRK-HOR                TO WRK-TMP-HOR.
BI0715     MOVE WRK-MIN                TO WRK-TMP-MIN.
BI0715     MOVE WRK-SEG                TO WRK-TMP-SEG.
BI0715     MOVE WRK-MIL                TO WRK-TMP-MIL.
BI0715     MOVE WRK-TIMESTAMP-AUX      TO HINCL-REG OF FD-IMAGX09.
BI0715
BI0715     MOVE FD-AGENCIA             TO CJUNC-DEPDC      OF FD-IMAGX09
BI0715     MOVE FD-CONTA               TO CCTA-CLI         OF FD-IMAGX09
BI0715     MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF     OF FD-IMAGX09
BI0715     MOVE 1947                   TO CFUNC-MANUT-INCL OF FD-IMAGX09
BI0715     MOVE ZEROS                  TO CFUNC-MANUT      OF FD-IMAGX09
BI0715     MOVE WRK-DATA-INIC-ADESAO   TO DINIC-ADSAO-INDVD
BI0715                                                     OF FD-IMAGX09
BI0715     MOVE '.'                           TO WRK-PTO5
BI0715                                           WRK-PTO6.
BI0715
BI0715     MOVE WRK-DATA-FIM-ADESAO    TO DFIM-ADSAO-INDVD OF FD-IMAGX09
BI0715     MOVE '0001-01-01-01.01.01.000001'
BI0715                                 TO HMANUT-REG       OF FD-IMAGX09
BI0715     MOVE ZEROS                  TO CINDCD-EXCL-REG  OF FD-IMAGX09
BI0715     MOVE ZEROS                  TO CTPO-SERVC-TARIF OF FD-IMAGX09
BI0715
BI0715     WRITE FD-IMAGX09.
BI0715
BI0715     MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
BI0715
BI0715     PERFORM 1132-TESTAR-FS-IMAGX09.
BI0715
BI0715     ADD 1                       TO ACU-GRAVADOS-IMAGX09.



           PERFORM 3171-GRAVAR-ADESAO.

      *----------------------------------------------------------------*
       3170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  GRAVAR ARQUIVO ADESAO                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3171-GRAVAR-ADESAO             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FD-ADESAO.

           MOVE FD-AGENCIA           TO FD-AGENCIA-AD      OF FD-ADESAO.
           MOVE FD-CONTA             TO FD-CONTA-AD        OF FD-ADESAO.
           MOVE FD-TARIFA-GFCT       TO FD-TARIFA-GFCT-AD  OF FD-ADESAO.
           MOVE FD-CENTRO-CUSTO      TO FD-CENTRO-CUSTO-AD OF FD-ADESAO.

           MOVE WRK-DIA-INI          TO WRK-DIA-INI-AD.
           MOVE WRK-MES-INI          TO WRK-MES-INI-AD.
           MOVE WRK-ANO-INI          TO WRK-ANO-INI-AD.
           MOVE WRK-DATA-INIC-AD     TO FD-DATA-INIC-VIG-AD.

           MOVE WRK-DIA-FIM          TO WRK-DIA-FIM-AD.
           MOVE WRK-MES-FIM          TO WRK-MES-FIM-AD.
           MOVE WRK-ANO-FIM          TO WRK-ANO-FIM-AD.
           MOVE WRK-DATA-FIM-AD      TO FD-DATA-FIM-VIG-AD.

           WRITE FD-ADESAO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1135-TESTAR-FS-ADESAO.

           ADD 1                       TO ACU-GRAVADOS-ADESAO.

      *----------------------------------------------------------------*
       3171-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMA MODULO SCCE9100 PARA VERIFICAR SE A AGENCIA/CONTA EH     *
      * VALIDA.                                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-CONSISTE-AG-CTA            SECTION.
      *----------------------------------------------------------------*

           MOVE FD-AGENCIA            TO LKW-CJUNC-DEPC.
           MOVE FD-CONTA              TO LKW-CTA-CORR.

           MOVE 'SCCE9100'             TO WRK-MODULO.

           CALL WRK-MODULO             USING LKW-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE RETURN-CODE        TO WRK-MSG02-CD
               MOVE WRK-MSG02          TO FD-MENSAGEM-RJ
               PERFORM 3100-GRAVAR-REJEITADOS
               GO TO   3200-99-FIM
           END-IF.

           IF  LKW-CD-RETORNO          NOT EQUAL ZEROS
               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE LKW-CD-RETORNO     TO WRK-MSG03-CD
               MOVE WRK-MSG03          TO FD-MENSAGEM-RJ
               PERFORM 3100-GRAVAR-REJEITADOS
               GO TO   3200-99-FIM
           END-IF.

           MOVE FD-AGENCIA      TO CJUNC-DEPDC      OF CLIEV008.
           MOVE FD-CONTA        TO CCTA-CLI         OF CLIEV008.

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
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_CORRENTE' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 3210-ACESSAR-CLIEV007
           ELSE
               MOVE 1                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV008
                                       TO CID-CLI          OF CLIEV004
               PERFORM 3220-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV007                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-ACESSAR-CLIEV007           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-AGENCIA     TO CJUNC-DEPDC      OF CLIEV007.
           MOVE FD-CONTA       TO CCTA-CLI         OF CLIEV007.

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
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_POUPANCA' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0040'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE WRK-MSG04          TO FD-MENSAGEM-RJ
               PERFORM 3100-GRAVAR-REJEITADOS
           ELSE
               MOVE 2                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV007
                                       TO CID-CLI          OF CLIEV004
               PERFORM 3220-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV004 PARA OBTER DADOS DO CLIENTE                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3220-ACESSAR-CLIEV004           SECTION.
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
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'V01CLIENTE_AGENCIA' TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0050'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE WRK-MSG04          TO FD-MENSAGEM-RJ
               PERFORM 3100-GRAVAR-REJEITADOS
           ELSE
               IF  CFLIAL-CGC OF CLIEV004
                                       EQUAL ZEROS
                   MOVE 'F'            TO WRK-TIPO-PESSOA-CLIENTE
               ELSE
                   MOVE 'J'            TO WRK-TIPO-PESSOA-CLIENTE
               END-IF
               PERFORM 3300-OBTER-DADOS-GFCTB0D8
           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA TARIFA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-OBTER-DADOS-GFCTB0D8       SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT          TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
             SELECT
                   CPSSOA_SERVC_TARIF,
                   CTPO_CTA_DEB_TARIF
             INTO
                   :GFCTB0D8.CPSSOA-SERVC-TARIF,
                   :GFCTB0D8.CTPO-CTA-DEB-TARIF
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE FD-ENTRADA         TO FD-REJEITADO
               MOVE WRK-MSG05          TO FD-MENSAGEM-RJ
               MOVE 0                  TO WRK-TIPO-CONTA-CLIENTE
               PERFORM 3100-GRAVAR-REJEITADOS
           END-IF.

BI1113     IF  (CTPO-CTA-DEB-TARIF OF GFCTB0D8  EQUAL  3)
BI1113     AND (WRK-TIPO-CONTA-CLIENTE          EQUAL  1)
BI1113         MOVE   CTPO-CTA-DEB-TARIF OF GFCTB0D8   TO
BI1113                WRK-TIPO-CONTA-CLIENTE
BI1113     END-IF.

           IF WRK-IGNORA-PERMISSAO EQUAL 'S'
              IF FD-TARIFA-GFCT EQUAL WRK-IGN-PERM-CESTA1 OR
                                      WRK-IGN-PERM-CESTA2 OR
                                      WRK-IGN-PERM-CESTA3 OR
                                      WRK-IGN-PERM-CESTA1 = 99999
                 MOVE 'S'            TO WRK-FIM-AGRUPAMENTO
                 MOVE 'S'            TO WRK-AGRUPAMENTO-VALIDO
              ELSE
                 MOVE 009            TO WRK-PROX-AGRUPAMENTO
                 MOVE 'N'            TO WRK-FIM-AGRUPAMENTO
                 MOVE 'N'            TO WRK-AGRUPAMENTO-VALIDO
              END-IF
           ELSE
              MOVE 009            TO WRK-PROX-AGRUPAMENTO
              MOVE 'N'            TO WRK-FIM-AGRUPAMENTO
              MOVE 'N'            TO WRK-AGRUPAMENTO-VALIDO
           END-IF.



           IF  CTPO-CTA-DEB-TARIF      OF GFCTB0D8
                                       EQUAL WRK-TIPO-CONTA-CLIENTE
               IF  CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL 3    OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL 1    AND
                   WRK-TIPO-PESSOA-CLIENTE         EQUAL 'F') OR
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL 2    AND
                   WRK-TIPO-PESSOA-CLIENTE         EQUAL 'J')
______*_______________MOVE_009____________TO_WRK-PROX-AGRUPAMENTO
______*_______________MOVE_'N'____________TO_WRK-FIM-AGRUPAMENTO
______*_______________MOVE_'N'____________TO_WRK-AGRUPAMENTO-VALIDO
                   PERFORM 3400-IDENTIFICAR-PERMISSAO UNTIL
                           WRK-FIM-AGRUPAMENTO        EQUAL 'S' OR
                           WRK-AGRUPAMENTO-VALIDO     EQUAL 'S'

                   IF  WRK-AGRUPAMENTO-VALIDO         EQUAL 'S'
ABR10                  IF  WRK-CESTA-CELULAR   EQUAL 'S'
ABR10                      MOVE FD-ENTRADA     TO FD-REJEITADO
ABR10                      MOVE WRK-MSG2311    TO FD-MENSAGEM-RJ
ABR10                      PERFORM 3100-GRAVAR-REJEITADOS
ABR10                  ELSE
                           MOVE  '0'       TO WRK-ERRO
ABR10                  END-IF
                   ELSE
                       IF  WRK-ERRO        =  ZEROS
                           MOVE FD-ENTRADA TO FD-REJEITADO
                           MOVE WRK-MSG11  TO FD-MENSAGEM-RJ
                           PERFORM 3100-GRAVAR-REJEITADOS
                       END-IF
                   END-IF
SP0404         ELSE
SP0404             IF  WRK-ERRO            =  ZEROS
SP0404                 MOVE FD-ENTRADA     TO FD-REJEITADO
SP0404                 MOVE WRK-MSG18      TO FD-MENSAGEM-RJ
SP0404
SP0404                 PERFORM 3100-GRAVAR-REJEITADOS
SP0404             END-IF
               END-IF
           ELSE
               IF  WRK-ERRO                =  ZEROS
                   MOVE FD-ENTRADA         TO FD-REJEITADO
                   MOVE WRK-MSG12          TO FD-MENSAGEM-RJ
                   PERFORM 3100-GRAVAR-REJEITADOS
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IDENTIFICAR AGRUPAMENTO COM PERMISSAO DE ADESAO AO PACOTE.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-IDENTIFICAR-PERMISSAO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-EXISTE-AGRUPAMENTO.

           EVALUATE WRK-PROX-AGRUPAMENTO

             WHEN 004
                  PERFORM 3405-OBTER-CNPJ-CPF-POSTO
                  IF  WRK-5527-CCGC-CPF               NOT EQUAL ZEROS
                      PERFORM 3415-ACESSAR-GFCTB0I0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 018         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 009
                  PERFORM 3420-OBTER-SEG-GESTAO

                  IF  GFCTJM-CSGMTO-GSTAO-TARIF       NOT EQUAL ZEROS
                      PERFORM 3425-ACESSAR-GFCTB0D2

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 014         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 014
                  PERFORM 3430-ACESSAR-GFCTB0C7

                  IF  WRK-EXISTE-AGRUPAMENTO          EQUAL 'S'
                      PERFORM 3410-OBTER-DADOS-GFCTB0C6
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 015         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 015
                  PERFORM 3405-OBTER-CNPJ-CPF-POSTO

                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3435-ACESSAR-GFCTB0D0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 017         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 017
                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3440-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 020         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 018
                  PERFORM 3442-OBTER-UF-MUNICIPIO

                  IF  GFCTRK-CMUN-RURAL               NOT EQUAL ZEROS
                      PERFORM 3445-ACESSAR-GFCTB0C9

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 019         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 019
                  IF  GFCTRK-CSGL-UF                  NOT EQUAL SPACES
                      PERFORM 3450-ACESSAR-GFCTB0D3

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 'S'         TO WRK-FIM-AGRUPAMENTO
                  END-IF

             WHEN 020
                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3440-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 004         TO WRK-PROX-AGRUPAMENTO
                  END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5527 PARA OBTER CPF/CNPJ/POSTO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       3405-OBTER-CNPJ-CPF-POSTO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO WRK-5527-AREA-SAIDA.

           INITIALIZE WRK-5527-AREA-SAIDA.

           MOVE 'GFCT1947'                 TO WRK-5527-CHAMADOR.
           MOVE FD-AGENCIA                 TO WRK-5527-AGENCIA-CLIENTE.
           MOVE FD-CONTA                   TO WRK-5527-CONTA-CLIENTE.
           MOVE 'B'                        TO WRK-5527-AMBIENTE.

           MOVE 'GFCT5527'                 TO WRK-MODULO.

           CALL WRK-MODULO                 USING WRK-5527-AREA-ENTRADA
                                                 WRK-5527-AREA-SAIDA
                                                 GFCT0M-AREA-ERROS
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE FD-ENTRADA             TO FD-REJEITADO
               MOVE RETURN-CODE            TO WRK-MSG07-CD
               MOVE WRK-MSG07              TO FD-MENSAGEM-RJ
               MOVE ZEROS                  TO WRK-5527-CCGC-CPF
                                              WRK-5527-CFLIAL-CGC
                                              WRK-5527-CCTRL-CPF-CGC
               IF  WRK-ERRO                =  ZEROS
                   PERFORM 3100-GRAVAR-REJEITADOS
               END-IF

           ELSE

               IF  WRK-5527-COD-RETORNO      NOT EQUAL ZEROS AND 02
                                                             AND 05
                   MOVE FD-ENTRADA           TO FD-REJEITADO
                   MOVE WRK-5527-COD-RETORNO TO WRK-MSG08-CD
                   MOVE WRK-MSG08            TO FD-MENSAGEM-RJ
                   MOVE ZEROS                TO WRK-5527-CCGC-CPF
                                                  WRK-5527-CFLIAL-CGC
                                                  WRK-5527-CCTRL-CPF-CGC
                   IF  WRK-ERRO              =  ZEROS
                       PERFORM 3100-GRAVAR-REJEITADOS
                   END-IF
               END-IF

               IF  WRK-5527-COD-RETORNO      EQUAL 02 OR 05
                   MOVE ZEROS                TO WRK-5527-CCGC-CPF
                                                WRK-5527-CFLIAL-CGC
                                                WRK-5527-CCTRL-CPF-CGC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3405-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA PERMISSAO DE ADESAO AO PACOTE.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3410-OBTER-DADOS-GFCTB0C6       SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE WRK-SEQ-AGPTO          TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
SP0404     MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
SP0404*--- MOVE WRK-DT-INICIO          TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
SP0404*--- MOVE DPROCM-ATUAL           OF GFCTB0A1
SP0404*---                             TO DFIM-PRMSS-PCOTE  OF GFCTB0C6.
BI0412     MOVE 'I'                    TO WRK-CINDCD-ADSAO-INDVD.
BI0412     MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C6.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
BI0412             CINDCD_VSLAO_COMP,
                   CINDCD_AGPTO_TOT
             INTO
                   :GFCTB0C6.CSEQ-AGPTO-CTA,
                   :GFCTB0C6.DINIC-PRMSS-PCOTE,
BI0412             :GFCTB0C6.CINDCD-VSLAO-COMP,
                   :GFCTB0C6.CINDCD-AGPTO-TOT
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF        = :GFCTB0C6.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0C6.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0C6.CSEQ-AGPTO-CTA
SP0404*---     AND DINIC_PRMSS_PCOTE   = :GFCTB0C6.DINIC-PRMSS-PCOTE
BI0412         AND (CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-INDVD OR
BI0412              CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-AMBAS)
PROC****_______AND_CIDTFD_PRMSS_INDVD__IN_(_0,_1_)
SP0404*---     AND DFIM_PRMSS_PCOTE   >= :GFCTB0C6.DFIM-PRMSS-PCOTE
SP0404         AND DINIC_PRMSS_PCOTE  <= :GFCTB0C6.DINIC-PRMSS-PCOTE
SP0404         AND DFIM_PRMSS_PCOTE   >= :GFCTB0C6.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'PRMSS_ADSAO_PCOTE'  TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0060'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 'S'                TO WRK-AGRUPAMENTO-VALIDO
           END-IF.

      *----------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO CLIENTE.*
      ******************************************************************
      *----------------------------------------------------------------*
       3415-ACESSAR-GFCTB0I0           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0I0.
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
ST25X6               CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST     AND
ST25X6               CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST   AND
ST25X6               CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0I0.DINIC-PRMSS-PCOTE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'TPRMSS_GRP_CLI  '   TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0070'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0I0 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0I0 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3415-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5534 PARA OBTER SEGMENTO GESTAO.            *
      ******************************************************************
      *----------------------------------------------------------------*
       3420-OBTER-SEG-GESTAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJM-AREA.

           INITIALIZE GFCTJM-AREA.

           MOVE 'B'                        TO GFCTJM-TPO-AMBIENTE.
           MOVE FD-AGENCIA                 TO GFCTJM-CJUNC-DEPDC.
           MOVE FD-CONTA                   TO GFCTJM-CCTA-CLI.

           MOVE 'GFCT5534'                 TO WRK-MODULO.

           CALL WRK-MODULO                 USING GFCTJM-AREA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE ZEROS                  TO GFCTJM-CSGMTO-GSTAO-TARIF
               IF  WRK-ERRO                =  ZEROS
                   MOVE FD-ENTRADA         TO FD-REJEITADO
                   MOVE RETURN-CODE        TO WRK-MSG09-CD
                   MOVE WRK-MSG09          TO FD-MENSAGEM-RJ
                   PERFORM 3100-GRAVAR-REJEITADOS
               END-IF

           ELSE

               IF  GFCTJM-COD-RETORNO      EQUAL 99
                   IF  WRK-ERRO            =  ZEROS
                       MOVE FD-ENTRADA         TO FD-REJEITADO
                       MOVE GFCTJM-COD-RETORNO TO WRK-MSG10-CD
                       MOVE WRK-MSG10          TO FD-MENSAGEM-RJ
                       PERFORM 3100-GRAVAR-REJEITADOS
                   END-IF
                   MOVE ZEROS              TO GFCTJM-CSGMTO-GSTAO-TARIF
               END-IF

               IF  GFCTJM-COD-RETORNO      NOT EQUAL ZEROS
                   MOVE ZEROS              TO GFCTJM-CSGMTO-GSTAO-TARIF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO SEGMENTO*
      ******************************************************************
      *----------------------------------------------------------------*
       3425-ACESSAR-GFCTB0D2           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0D2.
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
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'PRMSS_GRP_SGMTO '   TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0080'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D2 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D2 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3425-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO AGENCIA.*
      ******************************************************************
      *----------------------------------------------------------------*
       3430-ACESSAR-GFCTB0C7           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0C7.

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
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'PRMSS_GRP_DEPDC '   TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0090'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0C7 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0C7 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PAB.    *
      ******************************************************************
      *----------------------------------------------------------------*
       3435-ACESSAR-GFCTB0D0           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
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
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'PRMSS_GRP_PAB   '   TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0100'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D0 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D0 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3435-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PSTAL.  *
      ******************************************************************
      *----------------------------------------------------------------*
       3440-ACESSAR-GFCTB0D1           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0D1.
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
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'PRMSS_GRP_PSTAL '   TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0110'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D1 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D1 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5564 PARA OBTER UF E MUNICIPIO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       3442-OBTER-UF-MUNICIPIO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA.

           INITIALIZE GFCTRK-AREA.

           MOVE 'B'                    TO GFCTRK-TPO-AMBIENTE.
           MOVE FD-AGENCIA             TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               IF  WRK-ERRO                =  ZEROS
                   MOVE FD-ENTRADA             TO FD-REJEITADO
                   MOVE RETURN-CODE            TO WRK-MSG13-CD
                   MOVE WRK-MSG13              TO FD-MENSAGEM-RJ
                   PERFORM 3100-GRAVAR-REJEITADOS
               END-IF

           ELSE

               IF  GFCTRK-COD-RETORNO      EQUAL 01 OR 02 OR 99
                   IF  WRK-ERRO                =  ZEROS
                       MOVE FD-ENTRADA             TO FD-REJEITADO
                       MOVE GFCTRK-COD-RETORNO     TO WRK-MSG14-CD
                       MOVE WRK-MSG14              TO FD-MENSAGEM-RJ
                       PERFORM 3100-GRAVAR-REJEITADOS
                   END-IF
           END-IF.

           IF  GFCTRK-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       3442-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GP MUNICIPIO. *
      ******************************************************************
      *----------------------------------------------------------------*
       3445-ACESSAR-GFCTB0C9           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
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
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'PRMSS_GRP_MUN   '   TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0120'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0C9 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0C9 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3445-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO UF.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3450-ACESSAR-GFCTB0D3           SECTION.
      *----------------------------------------------------------------*

           MOVE FD-TARIFA-GFCT         TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0D3.
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
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'PRMSS_GRP_UF    '   TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0130'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D3 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D3 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA PROCESSAMENTO FINAL DO PROGRAMA           *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ENTRADA
                 REJEITA
                 ADESAO
                 IMAG009
BI0715           IMAGX09.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE EMISSAO DE DISPLAY DE TOTAIS                      *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-ENTRADA           NOT EQUAL ZEROS
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '*                   PROGRAMA GFCT1947
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
               MOVE ACU-GRAVADOS-IMAG009   TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS GRAVADOS  IMAG009  = '
                 WRK-MASCARA
                           ' *'
BI0715         MOVE ACU-GRAVADOS-IMAGX09   TO WRK-MASCARA
BI0715         DISPLAY '* TOTAL DE REGISTROS GRAVADOS  IMAGX09  = '
BI0715           WRK-MASCARA
BI0715                     ' *'
               MOVE ACU-GRAVADOS-REJEITA   TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS REJEITADOS         = '
                   WRK-MASCARA
                            ' *'
               MOVE ACU-GRAVADOS-ADESAO    TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS GRAVADOS  ADESAO   = '
                   WRK-MASCARA
                            ' *'
               DISPLAY '************************************************
      -        '*********'
           ELSE
               DISPLAY '*-----------------------------------------------
      -        '*'
               DISPLAY '*               PROGRAMA GFCT1947
      -        '*'
               DISPLAY '*               -----------------
      -        '*'
               DISPLAY '*
      -        '*'
               DISPLAY '*           ARQUIVO DE ENTRADA VAZIO
      -        '*'
               DISPLAY '*-----------------------------------------------
      -        '*'
           END-IF.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTE DE ERRRO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1947'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
