      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUCA6651.
       AUTHOR.     LEONARDO M CAMARGO.
      *================================================================*
      *                   B S I   T E C N O L O G I A                  *
      *----------------------------------------------------------------*
      *     PROGRAMA....: RUCA6651                                     *
      *     PROGRAMADOR.: LEONARDO M CAMARGO                           *
      *     ANALISTA....: EVANDRO GUIMARAES                            *
      *     DATA........: 08/2016                                      *
      *----------------------------------------------------------------*
      *     OBJETIVO....: INCLUSAO -                                   *
      *                   DESCLASSIFICACAO DE OPERACAO RURAL           *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     RUCAT651....: DESCLASSIFICACAO DE OPERACAO RURAL           *
      *----------------------------------------------------------------*
      *     BANCO DE DADOS :                                           *
      *----------------------------------------------------------------*
      *      DB2                                                       *
      *         TABLE                         INCLUDE/BOOK             *
      *         V01CONTRATO_RURAL               RUFIV002               *
      *         RUCA_CAD_ROOT                   RUCAB003               *
      *         TMOTVO_DCLAS_OPER               RURCB097               *
      *         TMOTVO_DCLAS_CONTR              RUFIB044               *
      *----------------------------------------------------------------*
      *     BOOKS.......:                                              *
      *----------------------------------------------------------------*
      *                                                                *
      *  - POL7100C    AREA P/ TRATAMENTO DE ERROS GRAVES              *
      *                                                                *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *----------------------------------------------------------------*
      *                                                                *
      *  - POOL5000    ENVIA/RECEBE MENSAGENS DO IMS(TELA)             *
      *  - POOL0660    INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *  - POOL7100    TRATAMENTO DE ERROS DE PROGRAMA                 *
      *  - POOL0432    DIGITO VERIFICADOR                              *
      *  - MESU9032    OBTER DADOS DA AGENCIA                          *
      *                                                                *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: RUCA6650                                      *
      *================================================================*
BRQ001*================================================================*
BRQ001*    PROGRAMADOR.:   GABRIEL SALIM            - BRQ              *
BRQ001*    ANALISTA....:   RODOLFO VIEIRA           - BRQ              *
BRQ001*    DATA........:   JULHO/2021                                  *
BRQ001*----------------------------------------------------------------*
BRQ001*    OBJETIVO....:  INCLUIR TRATAMENTO DE PARA ENVIO DA COR0003  *
BRQ001*================================================================*
WP1122*================================================================*
-     *    PROGRAMADOR.:   WIPRO                    - WIPRO            *
-     *    ANALISTA....:   CLAUDIO PIAZZA           - WIPRO            *
-     *    DATA........:   NOVEMBRO/2022                               *
-     *----------------------------------------------------------------*
-     *  OBJETIVO......:   ADEQUACOES SICOR 5.05 - MVP1                *
-     *                    INCLUSAO DO GRUPO DE DESCLASSIFICACAO 15    *
-     *                    CHAMADA RUEC8831 - FORMATA MENSAGEM COR0005 *
WP1122*================================================================*
7COMMP*================================================================*
-     *    PROGRAMADOR.:   7COMMP                   - 7COMM            *
-     *    ANALISTA....:   7COMMP                   - 7COMM            *
-     *    DATA........:   FEV/2023                                    *
-     *----------------------------------------------------------------*
-     *  OBJETIVO......:   PROCESSO DE DESCLASSIFICACAO                *
7COMMP*================================================================*
NAV506*    PROGRAMADOR.:   NAVA                                        *
NAV506*    ANALISTA....:   NAVA                                        *
NAV506*    DATA........:   MAI/2023                                    *
NAV506*----------------------------------------------------------------*
NAV506*  OBJETIVO......:   ADEQUACOES SICOR 5.06                       *
NAV506*================================================================*

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

       77  FILLER                      PIC  X(46)        VALUE
           '*** INICIO DA WORKING DO PROGRAMA RUCA6651 ***'.

      *--- AUXILIARES -------------------------------------------------*

       77  WRK-NOVA-OPCAO              PIC  9(02)        VALUE ZEROS.
       77  WRK-OPCAO-ATUAL             PIC  9(02)        VALUE ZEROS.
       77  WRK-GU                      PIC  X(04)        VALUE 'GU'.
       77  WRK-ISRT                    PIC  X(04)        VALUE 'ISRT'.
       77  WRK-CHNG                    PIC  X(04)        VALUE 'CHNG'.
       77  WRK-FUNCAO                  PIC  X(04)        VALUE SPACES.
       77  WRK-TELA                    PIC  X(08)        VALUE SPACES.

       77  WRK-INCONSISTENCIA          PIC  X(01)        VALUE SPACES.
       77  WRK-NAO-ENCONTRADO          PIC  X(01)        VALUE SPACES.
       77  WRK-DIGOPER                 PIC  X(01)        VALUE SPACES.
       77  WRK-NOME-AGENCIA            PIC  X(21)        VALUE SPACES.
       77  WRK-DEVEDOR                 PIC S9(13)V9(02)  USAGE COMP-3
                                                         VALUE ZEROS.
       77  WRK-ORIGEM-RECURSOS         PIC S9(03)V9(01)  USAGE COMP-3
                                                         VALUE ZEROS.
NAV506 77  WRK-TP-DESCLASS             PIC  X(01)        VALUE SPACES.
NAV506*01  ACU-ENVIO-COR0001           PIC S9(09) COMP   VALUE ZEROS.
NAV506*01  ACU-FETCH-COR0003           PIC S9(09) COMP   VALUE ZEROS.
NAV506*01  WRK-TOTAL-COR0003R1         PIC S9(09) COMP   VALUE ZEROS.
NAV506*01  WRK-CPF-X.
NAV506*    05 WRK-CPF-BAS              PIC  X(09)        VALUE SPACES.
NAV506*    05 FILLER                   PIC  X(04)        VALUE SPACES.
NAV506*    05 WRK-CPF-DIG              PIC  X(02)        VALUE SPACES.
NAV506*
NAV506*01  WRK-CGC-X.
NAV506*    05 WRK-CGC-BAS              PIC  X(09)        VALUE SPACES.
NAV506*    05 WRK-CGC-FILI             PIC  X(04)        VALUE SPACES.
NAV506*    05 WRK-CGC-DIG              PIC  X(02)        VALUE SPACES.
NAV506*
NAV506*01  WRK-PICX-15.
NAV506*    03 WRK-PIC9-15              PIC  9(15)        VALUE ZEROS.
BRQ001
BRQ001 01  WRK-RESTO                   PIC S9(18)V9(02) COMP-3
BRQ001                                                  VALUE ZEROS.

BRQ001 01  WRK-PIC9-5-CS               PIC +9(05)        VALUE ZEROS.
BRQ001 01  FILLER                      REDEFINES WRK-PIC9-5-CS.
BRQ001     03 FILLER                   PIC  X(01).
BRQ001     03 WRK-PIC9-5-SS            PIC  9(05).
BRQ001     03 FILLER REDEFINES WRK-PIC9-5-SS.
BRQ001        05 FILLER                PIC  9(02).
BRQ001        05 WRK-PIC9-3-SS         PIC  9(03).

NAV506*01  WRK-WCONTD-MSGEM-RURAL-AUX.
NAV506*    03 FILLER                   PIC  X(43)        VALUE SPACES.
NAV506*    03 WRK-NUMREFBCCOR-COR1     PIC  X(11)        VALUE SPACES.
NAV506*    03 FILLER                   PIC  X(32446)     VALUE SPACES.

BRQ001 01  WRK-VALOR-TOT-FINANC        PIC S9(014)V9(02) COMP-3
BRQ001                                                   VALUE ZEROS.
BRQ001 01  WRK-VALOR-DESCL-ATU         PIC S9(014)V9(02) COMP-3
BRQ001                                                   VALUE ZEROS.

       01  WRK-TIMESTAMP               PIC  X(26)        VALUE SPACES.
       01  WRK-MODULO                  PIC  X(08)        VALUE SPACES.

       01  FILLER.
           03  WRK-CUSUAR-MANUT-NULL   PIC S9(04) COMP   VALUE ZEROS.
           03  WRK-HMANUT-NULL         PIC S9(04) COMP   VALUE ZEROS.
BRQ001     03  WRK-CREFT-RECOR-NULL    PIC S9(04) COMP   VALUE ZEROS.
NAV506*    03  WRK-CMSGEM-RECOR-NULL   PIC S9(04) COMP   VALUE ZEROS.

       01  WRK-COD-USER                PIC  9(07)        VALUE ZEROS.
       01  WRK-COD-USER-X              REDEFINES
           WRK-COD-USER                PIC  X(07).

       01  WRK-DESC-CGC.
           03  WRK-RAIZ-CGC            PIC 9(09)         VALUE ZEROS.
           03  WRK-FILIAL-CGC          PIC 9(04)         VALUE ZEROS.
           03  WRK-DIG-CGC             PIC 9(02)         VALUE ZEROS.
       01  WRK-DESC-CGC-R  REDEFINES  WRK-DESC-CGC PIC 9(15).

       01  WRK-MENSAGEM.
           03  FILLER                  PIC  X(04)        VALUE SPACES.
           03  WRK-TRANSACAO           PIC  X(08)        VALUE SPACES.
           03  FILLER                  PIC  X(1988)      VALUE SPACES.

       01  WRK-COD-DEPTO.
           03  FILLER                  PIC  9(02)        VALUE ZEROS.
           03  WRK-COD-DEPTO-N         PIC  9(04)        VALUE ZEROS.

      *--- AREA DA POOL0432 -------------------------------------------*

       01  WRK-AREA-POOL0432.
           03  WRK-DIGITO-0432         PIC  X(01)        VALUE SPACES.
           03  WRK-TAMANHO-0432        PIC  9(02)        VALUE ZEROS.
           03  WRK-VALOR-0432          PIC  9(09)        VALUE ZEROS.

      *---------------------------------------------------------------*
      ***  DEFINICAO   DA  POOL0431
      *---------------------------------------------------------------*
       01  FILLER.

           05  WRK-DIGITO-COSA         PIC  X(002)       VALUE SPACES.
           05  WRK-DIGITO-COSA-RED     REDEFINES  WRK-DIGITO-COSA.
               07  WRK-DIGITO-COSA-1   PIC  X(001).
               07  WRK-DIGITO-COSA-2   PIC  X(001).

       77  WRK-TAMANHO-0431            PIC  9(002)       VALUE  7.

      *
      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO MESU9032 ***'.
      *---------------------------------------------------------------*
       01  WRK-MESU9032.
           03  WRK-ENTRADA-MESU9032.
               05  WRK-NOME-PGM        PIC X(08)     VALUE SPACES.
               05  WRK-COD-RETORNO     PIC 9(02)     VALUE ZEROS.
               05  WRK-AGENCIA         PIC 9(05)     VALUE ZEROS.
           03  WRK-SAIDAS-MESU9032.
               05  WRK-NOME            PIC X(20)     VALUE SPACES.
               05  WRK-ENDERECO        PIC X(30)     VALUE SPACES.
               05  WRK-CIDADE          PIC X(25)     VALUE SPACES.
               05  WRK-ESTADO          PIC X(02)     VALUE SPACES.
               05  WRK-CEP             PIC 9(05)     VALUE ZEROS.
               05  WRK-REGIO-DIRET     PIC 9(03)     VALUE ZEROS.
               05  WRK-JUNC-REGIO      PIC 9(05)     VALUE ZEROS.
               05  WRK-NOVOCEP         PIC 9(09)     VALUE ZEROS.
               05  WRK-NOME-GER        PIC X(30)     VALUE SPACES.
               05  WRK-DT-INAUGUR      PIC X(10)     VALUE SPACES.
               05  WRK-DT-ENCRESU      PIC X(10)     VALUE SPACES.
               05  WRK-LOJA            PIC 9(05)     VALUE ZEROS.
               05  WRK-CONTROLE        PIC X(02)     VALUE SPACES.

       01  WRK-POOL7100-MESU9032       PIC X(107)    VALUE SPACES.
       01  WRK-SQLCA-MESU9032          PIC X(136)    VALUE SPACES.
       01  WRK-AMBIENTE-MESU9032       PIC X(01)     VALUE SPACES.
       01  WRK-IO-PCB-MESU9032         PIC X(32)     VALUE SPACES.
       01  WRK-ALT-PCB-MESU9032        PIC X(32)     VALUE SPACES.

      *--- MENSAGEM ERRO POOL0660 -------------------------------------*

       01  WRK-ERRO-POOL0660.
           03  FILLER                  PIC  X(47)        VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA POOL0660 *'.
           03  FILLER                  PIC  X(12)        VALUE
               '* RET.COD = '.
           03  WRK-RETURN-CODE         PIC  9(02)BB      VALUE ZEROS.
           03  FILLER                  PIC  X(08)        VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO          PIC  9(02)        VALUE ZEROS.

      *--- AREA DE ATRIBUTOS ------------------------------------------*

       01  WRK-ATRIBUTOS.

      *****************************************************************
      *    BRILHANTE, DESPROT COM POS.CURSOR NUM.                     *
      *               -------- -- ----                                *
               03  WRK-49369           PIC S9(008) COMP VALUE +49369.
               03  WRK-49361           PIC S9(008) COMP VALUE +49361.
               03  WRK-225             PIC S9(008) COMP VALUE +225.
      *****************************************************************

      *--- MENSAGENS EMITIDAS PELO PROGRAMA ---------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG001              PIC  X(79)        VALUE
               'INFORME A CHAVE DA OPERACAO E TECLE ENTER.'.
           03  WRK-MSG002              PIC  X(79)        VALUE
               'TECLA DE FUNCAO (PF) NAO VALIDA OU NAO DISPONIVEL.'.

201609*----------------------------------------------------------------*
201609 01  FILLER                      PIC  X(032)         VALUE
201609     '*** COMUNICACAO COM RURC2770 ***'.
201609*----------------------------------------------------------------*
201609
201609 01  WRK-2770-AREA.
201609     05  WRK-2770-LL             PIC S9(004) COMP  VALUE ZEROS.
201609     05  WRK-2770-ZZ             PIC S9(004) COMP  VALUE ZEROS.
201609     05  WRK-2770-TRANCODE.
201609         10  WRK-2770-TRANSACAO  PIC  X(008)       VALUE SPACES.
201609         10  FILLER              PIC  X(006)       VALUE SPACES.
201609         10  WRK-2770-PFK        PIC  X(001)       VALUE SPACES.
201609     05  WRK-2770-SENHAS         PIC  X(037)       VALUE SPACES.
201609     05  WRK-2770-OPCAO          PIC  9(002)       VALUE ZEROS.
201609     05  WRK-2770-MOTIVO-SELCD   PIC  X(002)       VALUE SPACES.
201609     05  WRK-2770-MOTIVO-SELCD-N REDEFINES
201609         WRK-2770-MOTIVO-SELCD   PIC  9(002).
201609     05  WRK-2770-MENSAGEM       PIC  X(079)       VALUE SPACES.
201609     05  WRK-2770-ANTE-TELA      PIC  X(008)       VALUE SPACES.
201609     05  WRK-2770-NAVEGACAO      PIC  X(300)       VALUE SPACES.
201609

      *----------------------------------------------------------------*
      ***  TELA RUCAT651            - INPUT
      *----------------------------------------------------------------*

       01  WRK-INPUT-T651.
           03  WRK-FILLER-T651             PIC  X(18) VALUE SPACES.
           03  WRK-INP-PFK-T651            PIC  X(01) VALUE SPACES.
           03  WRK-INP-SENHAS-T651         PIC  X(37) VALUE SPACES.
           03  WRK-INP-NOPCAO-T651         PIC  X(02) VALUE SPACES.
           03  WRK-INP-FASE-T651            PIC  X(01) VALUE SPACES.
           03  WRK-INP-NROPER-T651-X.
               05  WRK-INP-NROPER-T651      PIC  9(09) VALUE ZEROS.
           03  WRK-INP-DADOS-T651.
               05  WRK-INP-DIGOPER-T651     PIC  X(01) VALUE SPACES.
BRQ001         05  WRK-INP-CTRLIF-T651      PIC  X(20) VALUE SPACES.
BRQ001         05  WRK-INP-NMOTVO-T651      PIC  9(05) VALUE ZEROS.
               05  WRK-INP-AGENCIA-T651-X.
                   07  WRK-INP-AGENCIA-T651 PIC  9(05) VALUE ZEROS.
               05  WRK-INP-NOMAG-T651       PIC  X(20) VALUE SPACES.
               05  WRK-INP-AAFINAN-T651-X.
                   07  WRK-INP-AAFINAN-T651 PIC  9(07) VALUE ZEROS.
               05  WRK-INP-NOME-T651        PIC  X(30) VALUE SPACES.
               05  WRK-INP-CC-T651-X.
                   07  WRK-INP-CC-T651      PIC  9(07) VALUE ZEROS.
               05  WRK-INP-CCDIG-T651-X.
                   07  WRK-INP-CCDIG-T651   PIC  9(01) VALUE ZEROS.
               05  WRK-INP-CGCR-T651-X.
                   07  WRK-INP-CGCR-T651    PIC  9(09) VALUE ZEROS.
               05  WRK-INP-CGCF-T651-X.
                   07  WRK-INP-CGCF-T651    PIC  9(04) VALUE ZEROS.
               05  WRK-INP-CGCD-T651-X.
                   07  WRK-INP-CGCD-T651    PIC  9(02) VALUE ZEROS.
               05  WRK-INP-CREDITO-T651     PIC  X(21) VALUE SPACES.
               05  WRK-INP-DEVEDOR-T651     PIC  X(21) VALUE SPACES.
               05  WRK-INP-VLRDESC-T651     PIC  X(21) VALUE SPACES.
               05  WRK-INP-DTDESC-T651      PIC  X(10) VALUE SPACES.
               05  WRK-INP-MOTDES-T651-X.
                   07  WRK-INP-MOTDES-T651  PIC  9(03) VALUE ZEROS.
               05  WRK-INP-MOTDES1-T651     PIC  X(60) VALUE SPACES.
           03  WRK-INP-MENSAG-T651          PIC  X(79) VALUE SPACES.
           03  WRK-INP-T651.
201609         05  WRK-INP-LIN-T651         PIC S9(02) COMP VALUE ZEROS.
201609         05  WRK-INP-COL-T651         PIC S9(02) COMP VALUE ZEROS.

      /----------------------------------------------------------------*
      ***  TELA RUCAT651            - OUTPUT
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-T651.
           03  WRK-OUT-LL-T651              PIC S9(04) COMP VALUE ZEROS.
           03  WRK-OUT-ZZ-T651              PIC S9(04) COMP VALUE ZEROS.
           03  WRK-OUT-SENHAS-T651          PIC  X(37) VALUE SPACES.
           03  WRK-OUT-NOPCAO-T651          PIC  X(02) VALUE SPACES.
           03  WRK-OUT-FASE-T651            PIC  X(01) VALUE SPACES.
           03  WRK-OUT-OPER-ATTR-T651       PIC  9(04) COMP VALUE ZEROS.
           03  WRK-OUT-NROPER-T651-X.
               05  WRK-OUT-NROPER-T651      PIC  9(09) VALUE ZEROS.
           03  WRK-OUT-DADOS-T651.
               05  WRK-OUT-DIGOPER-T651     PIC  X(01) VALUE SPACES.
BRQ001         05  WRK-OUT-CTRLIF-T651      PIC  X(20) VALUE SPACES.
BRQ001         05  WRK-OUT-NMOTVO-T651      PIC  9(05) VALUE ZEROS.
               05  WRK-OUT-AGENCIA-T651-X.
                   07  WRK-OUT-AGENCIA-T651 PIC  9(05) VALUE ZEROS.
               05  WRK-OUT-NOMAG-T651       PIC  X(20) VALUE SPACES.
               05  WRK-OUT-AAFINAN-T651-X.
                   07  WRK-OUT-AAFINAN-T651 PIC  9(07) VALUE ZEROS.
               05  WRK-OUT-NOME-T651        PIC  X(30) VALUE SPACES.
               05  WRK-OUT-CC-T651-X.
                   07  WRK-OUT-CC-T651      PIC  9(07) VALUE ZEROS.
               05  WRK-OUT-CCDIG-T651-X.
                   07  WRK-OUT-CCDIG-T651   PIC  9(01) VALUE ZEROS.
               05  WRK-OUT-CGCR-T651-X.
                   07  WRK-OUT-CGCR-T651    PIC  9(09) VALUE ZEROS.
               05  WRK-OUT-CGCF-T651-X.
                   07  WRK-OUT-CGCF-T651    PIC  9(04) VALUE ZEROS.
               05  WRK-OUT-CGCD-T651-X.
                   07  WRK-OUT-CGCD-T651    PIC  9(02) VALUE ZEROS.
               05  WRK-OUT-CREDITO-T651-X.
                   07  WRK-OUT-CREDITO-T651 PIC  ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                       VALUE ZEROS.
               05  WRK-OUT-DEVEDOR-T651-X.
                   07  WRK-OUT-DEVEDOR-T651 PIC  ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                       VALUE ZEROS.
               05  WRK-OUT-VLRDESC-ATTR-T651 PIC 9(04) COMP VALUE ZEROS.
               05  WRK-OUT-VLRDESC-T651-X.
                   07  WRK-OUT-VLRDESC-T651 PIC  ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                       VALUE ZEROS.
               05  WRK-OUT-DTDESC-ATTR-T651 PIC  9(04) COMP VALUE ZEROS.
               05  WRK-OUT-DTDESC-T651      PIC  X(10) VALUE SPACES.
               05  WRK-OUT-MOTDES-ATTR-T651 PIC  9(04) COMP VALUE ZEROS.
               05  WRK-OUT-MOTDES-T651-X.
                   07  WRK-OUT-MOTDES-T651  PIC  9(03) VALUE ZEROS.
               05  WRK-OUT-MOTDES1-T651     PIC  X(60) VALUE SPACES.
           03  WRK-OUT-MENSAG-T651          PIC  X(79) VALUE SPACES.

      *----------------------------------------------------------------*
      ***  AREA DA POOL0660 COM MASCARA DA TELA RUCAT651 - SEGMENTO 00
      *----------------------------------------------------------------*

       01  WRK-660-RUCAT651.
           03  WRK-660-LL-AREA-T651         PIC  9(04) COMP VALUE ZEROS.
           03  WRK-660-LL-MENSAGEM-T651     PIC  9(04) COMP VALUE ZEROS.
           03  WRK-660-SENHAS-T651          PIC  9(04) COMP VALUE 1037.
           03  WRK-660-NOPCAO-T651          PIC  9(04) COMP VALUE 0002.
           03  WRK-660-FASE-T651            PIC  9(04) COMP VALUE 0001.
           03  WRK-660-NROPER-T651          PIC  9(04) COMP VALUE 2011.
           03  WRK-660-DIGOPER-T651         PIC  9(04) COMP VALUE 0001.
BRQ001     03  WRK-660-CTRLIF-T651          PIC  9(04) COMP VALUE 0020.
BRQ001     03  WRK-660-NMOTVO-T651          PIC  9(04) COMP VALUE 0005.
           03  WRK-660-AGENCIA-T651         PIC  9(04) COMP VALUE 0005.
           03  WRK-660-NOMAG-T651           PIC  9(04) COMP VALUE 0020.
           03  WRK-660-AAFINAN-T651         PIC  9(04) COMP VALUE 0007.
           03  WRK-660-NOME-T651            PIC  9(04) COMP VALUE 0030.
           03  WRK-660-CC-T651              PIC  9(04) COMP VALUE 0007.
           03  WRK-660-CCDIG-T651           PIC  9(04) COMP VALUE 0001.
           03  WRK-660-CGCR-T651            PIC  9(04) COMP VALUE 0009.
           03  WRK-660-CGCF-T651            PIC  9(04) COMP VALUE 0004.
           03  WRK-660-CGCD-T651            PIC  9(04) COMP VALUE 0002.
           03  WRK-660-CREDITO-T651         PIC  9(04) COMP VALUE 0021.
           03  WRK-660-DEVEDOR-T651         PIC  9(04) COMP VALUE 0021.
           03  WRK-660-VLRDESC-T651         PIC  9(04) COMP VALUE 2023.
           03  WRK-660-DTDESC-T651          PIC  9(04) COMP VALUE 2012.
           03  WRK-660-MOTDES-T651          PIC  9(04) COMP VALUE 2005.
           03  WRK-660-MOTDES1-T651         PIC  9(04) COMP VALUE 0060.
           03  WRK-660-MENSAG-T651          PIC  9(04) COMP VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA POOL1205         ***'.
      *----------------------------------------------------------------*

       01  WRK-1205-MENSAGEM           PIC  X(50)          VALUE SPACES.
      *
       01  WRK-AREA-POOL1205.
           03  WRK-1205-DATA          PIC  9(09) COMP-3   VALUE ZEROS.
           03  WRK-1205-OPCAO         PIC  X(01)          VALUE SPACES.
           03  WRK-1205-JULIANA       PIC  9(07) COMP-3   VALUE ZEROS.
           03  WRK-1205-EDITADA       PIC  X(10)          VALUE SPACES.
           03  WRK-1205-GREGORI       PIC  9(08)          VALUE ZEROS.
           03  WRK-1205-DIA-SEMANA    PIC  X(13)          VALUE SPACES.
           03  WRK-1205-MES-EXTENSO   PIC  X(09)          VALUE SPACES.
           03  WRK-1205-DIA-UT-ANTER  PIC  9(08)          VALUE ZEROS.
           03  WRK-1205-DIA-UT-POST   PIC  9(08)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA AUXILIAR            ***'.
      *----------------------------------------------------------------*

       01  WRK-CH21-GRUPO.
           03 FILLER                   PIC  X(005)         VALUE SPACES.
           03 WRK-ZD14V2               PIC  9(014)V9(02)   VALUE ZEROS.
       01  WRK-CH21                    REDEFINES
           WRK-CH21-GRUPO              PIC  X(021).

       01  FILLER.
           03  WRK-VALOR-DESCL         PIC  9(014)V9(02)   VALUE ZEROS.
           03  WRK-DATA-DESCL          PIC  X(010)         VALUE SPACES.

       01  FILLER.
           03  WRK-DATA-ISO            PIC  9(008)         VALUE ZEROS.
           03  FILLER                  REDEFINES WRK-DATA-ISO.
               05  WRK-ANO-ISO         PIC  X(004).
               05  WRK-MES-ISO         PIC  X(002).
               05  WRK-DIA-ISO         PIC  X(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA POOL0496         ***'.
      *----------------------------------------------------------------*

       01  WRK-POOL0496-AREA.
           03  WRK-POOL0496-ENTRADA        PIC  X(21)  VALUE SPACES.
           03  WRK-POOL0496-SAIDA          PIC  X(21)  VALUE SPACES.
           03  WRK-POOL0496-TAMANHO.
               05 WRK-POOL0496-TAM-ENT     PIC  9(02)  VALUE 21.
               05 WRK-POOL0496-TAM-SAI     PIC  9(02)  VALUE 21.
               05 WRK-POOL0496-TAM-INT     PIC  9(02)  VALUE ZEROS.
               05 WRK-POOL0496-TAM-DEC     PIC  9(02)  VALUE ZEROS.

       01  FILLER.
           03 WRK-POOL0496-RETURN-CODE PIC S9(009) COMP VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AUXILIAR NOME AGENCIAS   ***'.
      *----------------------------------------------------------------*

       01 FILLER.
           03 WRK-TABELA.
               05 FILLER               PIC X(41) VALUE
                  '0027CALCADA-SALVADOR     15.142.490000219'.
               05 FILLER               PIC X(41) VALUE
                  '0035CEASA-SALVADOR       15.142.490000308'.
               05 FILLER               PIC X(41) VALUE
                  '0060RIO DE JANEIRO       15.142.490000642'.
               05 FILLER               PIC X(41) VALUE
                  '0078ALAGOINHAS           15.142.490000723'.
               05 FILLER               PIC X(41) VALUE
                  '0086BARREIRAS            15.142.490000804'.
               05 FILLER               PIC X(41) VALUE
                  '0094BOM JESUS DA LAPA    15.142.490000995'.
               05 FILLER               PIC X(41) VALUE
                  '0108BREJOES              15.142.490001029'.
               05 FILLER               PIC X(41) VALUE
                  '0116CAMACA               15.142.490001100'.
               05 FILLER               PIC X(41) VALUE
                  '0132CANCEICAO DO ALMEIDA 15.142.490001371'.
               05 FILLER               PIC X(41) VALUE
                  '0140ESPLANADA            15.142.490001452'.
               05 FILLER               PIC X(41) VALUE
                  '0159EUCLIDES DA CUNHA    15.142.490001533'.
               05 FILLER               PIC X(41) VALUE
                  '0167FEIRA DE SANTANA     15.142.490001614'.
               05 FILLER               PIC X(41) VALUE
                  '0175GANDU                15.142.490001703'.
               05 FILLER               PIC X(41) VALUE
                  '0186IGUAI                15.142.490001886'.
               05 FILLER               PIC X(41) VALUE
                  '0191ILHEUS               15.142.490001967'.
               05 FILLER               PIC X(41) VALUE
                  '0205IPIAU                15.142.490002009'.
               05 FILLER               PIC X(41) VALUE
                  '0213ITABEABA             15.142.490002101'.
               05 FILLER               PIC X(41) VALUE
                  '0631ITABUNA              15.142.490002262'.
               05 FILLER               PIC X(41) VALUE
                  '0240ITIRUCU              15.142.490002424'.
               05 FILLER               PIC X(41) VALUE
                  '0256ITORORO              15.142.490002505'.
               05 FILLER               PIC X(41) VALUE
                  '0264JEQUIE               15.142.490002696'.
               05 FILLER               PIC X(41) VALUE
                  '0272JUAZEIRO             15.142.490002777'.
               05 FILLER               PIC X(41) VALUE
                  '0280LIVRAMENTO N. SENHORA15.142.490002850'.
               05 FILLER               PIC X(41) VALUE
                  '0310MEDEIROS NETO        15.142.490003153'.
               05 FILLER               PIC X(41) VALUE
                  '0329MORRO DO CHAPEU      15.142.490003234'.
               05 FILLER               PIC X(41) VALUE
                  '0337MURITIBA             15.142.490003316'.
               05 FILLER               PIC X(41) VALUE
                  '0345NAZARE               15.142.490003404'.
               05 FILLER               PIC X(41) VALUE
                  '0353PARAMIRIM            15.142.490003587'.
               05 FILLER               PIC X(41) VALUE
                  '0361PORTO SEGURO         15.142.490003668'.
               05 FILLER               PIC X(41) VALUE
                  '0370POTIRAGUA            15.142.490003749'.
               05 FILLER               PIC X(41) VALUE
                  '0389RUY BARBOSA          15.142.490003820'.
               05 FILLER               PIC X(41) VALUE
                  '0396STA MARIA VITORIA    15.142.490003900'.
               05 FILLER               PIC X(41) VALUE
                  '0400SANTANA              15.142.490004044'.
               05 FILLER               PIC X(41) VALUE
                  '0418SANTO ANTONIO JESUS  15.142.490004125'.
               05 FILLER               PIC X(41) VALUE
                  '0426SEABRA               15.142.490004206'.
               05 FILLER               PIC X(41) VALUE
                  '0434SENHOR DO BONFIM     15.142.490004397'.
               05 FILLER               PIC X(41) VALUE
                  '0442TUCANO               15.142.490004479'.
               05 FILLER               PIC X(41) VALUE
                  '0485VITORIA DA CONQUISTA 15.142.490004800'.
               05 FILLER               PIC X(41) VALUE
                  '0493XIQUE-XIQUE          15.142.490004982'.
               05 FILLER               PIC X(41) VALUE
                  '0507SAO PEDRO-SALVADOR   15.142.490005016'.
               05 FILLER               PIC X(41) VALUE
                  '0515SHOPING SBS          15.142.490005105'.
               05 FILLER               PIC X(41) VALUE
                  '0523ITAPETINGA           15.142.490005280'.
               05 FILLER               PIC X(41) VALUE
                  '0540GUANAMBI             15.142.490005440'.
               05 FILLER               PIC X(41) VALUE
                  '0574CAMPO GRANDE-SDOR    15.142.490005792'.
               05 FILLER               PIC X(41) VALUE
                  '0582CONCEICAO DO JACUIPE 15.142.490005873'.
               05 FILLER               PIC X(41) VALUE
                  '0590ROQUIRA              15.142.490005954'.
               05 FILLER               PIC X(41) VALUE
                  '0604JOCOBINA             15.142.490006098'.
               05 FILLER               PIC X(41) VALUE
                  '0612POJUCA               15.142.490006171'.
               05 FILLER               PIC X(41) VALUE
                  '0620SANTO ESTEVAO        15.142.490006250'.
               05 FILLER               PIC X(41) VALUE
                  '0639SAO FCO DO CONDE     15.142.490006330'.
               05 FILLER               PIC X(41) VALUE
                  '0647TEIXEIRA DE FREITAS  15.142.490006411'.
               05 FILLER               PIC X(41) VALUE
                  '0671CENTRO-SALVADOR      15.142.490006764'.
               05 FILLER               PIC X(41) VALUE
                  '0680CATU                 15.142.490006845'.
               05 FILLER               PIC X(41) VALUE
                  '0692URUCUCA              15.142.490006926'.
               05 FILLER               PIC X(41) VALUE
                  '0701JEREMOABO            15.142.490007060'.
               05 FILLER               PIC X(41) VALUE
                  '0710CAB-SALVADOR         15.142.490007140'.
               05 FILLER               PIC X(41) VALUE
                  '0728ENCRUZILHADA         15.142.490007631'.
               05 FILLER               PIC X(41) VALUE
                  '0752SANTA INES           15.142.490007574'.
               05 FILLER               PIC X(41) VALUE
                  '0760RIACHO DO JACUIPE    15.142.490007658'.
               05 FILLER               PIC X(41) VALUE
                  '0779FLORESTA AZUL        15.142.490007736'.
               05 FILLER               PIC X(41) VALUE
                  '0787RIO REAL             15.142.490007817'.
               05 FILLER               PIC X(41) VALUE
                  '0795CAMACARI             15.142.490007906'.
               05 FILLER               PIC X(41) VALUE
                  '0809CONDEUBA             15.142.490008031'.
               05 FILLER               PIC X(41) VALUE
                  '0817COTEGIPE             15.142.490008112'.
               05 FILLER               PIC X(41) VALUE
                  '0826MUCURI               15.142.490008201'.
               05 FILLER               PIC X(41) VALUE
                  '0833BROTAS DE MACAUBA    15.142.490008394'.
               05 FILLER               PIC X(41) VALUE
                  '0841SOBRADINHO           15.142.490008465'.
               05 FILLER               PIC X(41) VALUE
                  '0868CORRENTINA           15.142.490008627'.
               05 FILLER               PIC X(41) VALUE
                  '0892TAPIRAMUTA           15.142.490008970'.
               05 FILLER               PIC X(41) VALUE
                  '0930IGUATEMI             15.142.490009356'.
               05 FILLER               PIC X(41) VALUE
                  '0949ITAJU DO COLONIA     15.142.490009437'.
               05 FILLER               PIC X(41) VALUE
                  '0957LAGEDAO              15.142.490009519'.
               05 FILLER               PIC X(41) VALUE
                  '0965DARIO MEIRA          15.142.490009607'.
               05 FILLER               PIC X(41) VALUE
                  '0973ITAGIMIRIM           15.142.490009780'.
               05 FILLER               PIC X(41) VALUE
                  '0981NILO PECANHA         15.142.490009860'.
               05 FILLER               PIC X(41) VALUE
                  '1031BARRA DO CHOCA       15.142.490010362'.
               05 FILLER               PIC X(41) VALUE
                  '1040BOM DESPACHO         15.142.490010443'.
               05 FILLER               PIC X(41) VALUE
                  '1074RIO DO PIRES         15.142.490010796'.
               05 FILLER               PIC X(41) VALUE
                  '1090SAUDE                15.142.490010958'.
               05 FILLER               PIC X(41) VALUE
                  '1155ABATE                15.142.490011504'.
               05 FILLER               PIC X(41) VALUE
                  '1163WAGNER               15.142.490011667'.
               05 FILLER               PIC X(41) VALUE
                  '1171VARZEA DO POCO       15.142.490011768'.
               05 FILLER               PIC X(41) VALUE
                  '1198RIBEIRA DO POMBAL    15.142.490011920'.
               05 FILLER               PIC X(41) VALUE
                  '1210MONTE SANTO          15.142.490012144'.
               05 FILLER               PIC X(41) VALUE
                  '1228PIRIPA               15.142.490016325'.
               05 FILLER               PIC X(41) VALUE
                  '1244ITUACU               15.142.490012497'.
               05 FILLER               PIC X(41) VALUE
                  '1287PRES.JANIO QUADROS   15.142.490012810'.
               05 FILLER               PIC X(41) VALUE
                  '1295ARACATU              15.142.490012900'.
               05 FILLER               PIC X(41) VALUE
                  '1325TANQUE NOVO          15.142.490013206'.
               05 FILLER               PIC X(41) VALUE
                  '1333BREJOLANDIA          15.142.490013380'.
               05 FILLER               PIC X(41) VALUE
                  '1341PIATA                15.142.490013469'.
               05 FILLER               PIC X(41) VALUE
                  '1350IRECE                15.142.490013540'.
               05 FILLER               PIC X(41) VALUE
                  '1376AMARGOSA             15.142.490013701'.
               05 FILLER               PIC X(41) VALUE
                  '1406CRISTOPOLIS          15.142.490014007'.
               05 FILLER               PIC X(41) VALUE
                  '1414MAIQUINIQUE          15.142.490014199'.
               05 FILLER               PIC X(41) VALUE
                  '1422ITAMARAJU            15.142.490014279'.
               05 FILLER               PIC X(41) VALUE
                  '1430MACURURE             15.142.490014350'.
               05 FILLER               PIC X(41) VALUE
                  '1473BRUMADO              15.142.490014783'.
               05 FILLER               PIC X(41) VALUE
                  '1481VALENCA              15.142.490014864'.
               05 FILLER               PIC X(41) VALUE
                  '1503BARRA-SALVADOR       15.142.490015089'.
               05 FILLER               PIC X(41) VALUE
                  '1520IPUPIARA             15.142.490015240'.
               05 FILLER               PIC X(41) VALUE
                  '1538CONTENDAS SINCORA    15.142.490015321'.
               05 FILLER               PIC X(41) VALUE
                  '1554SOUTO SOARES         15.142.490015593'.
               05 FILLER               PIC X(41) VALUE
                  '1562JAGUARARI            15.142.490015674'.
               05 FILLER               PIC X(41) VALUE
                  '1589CACULE               15.142.490015836'.
               05 FILLER               PIC X(41) VALUE
                  '1597CORDEIROS            15.142.490015917'.
               05 FILLER               PIC X(41) VALUE
                  '1600RODELAS              15.142.490016050'.
               05 FILLER               PIC X(41) VALUE
                  '1619PLANALTINO           15.142.490016131'.
               05 FILLER               PIC X(41) VALUE
                  '1635CAETITE              15.142.490016201'.
               05 FILLER               PIC X(41) VALUE
                  '1651LAMARAO              15.142.490016565'.
               05 FILLER               PIC X(41) VALUE
                  '1686TEOLANDIA            15.142.490016808'.
               05 FILLER               PIC X(41) VALUE
                  '1708TABOCAS DO B. VELHO  15.142.490017022'.
               05 FILLER               PIC X(41) VALUE
                  '1716CORIBE               15.142.490017103'.
               05 FILLER               PIC X(41) VALUE
                  '1724PEDRO ALEXANDRE      15.142.490017284'.
               05 FILLER               PIC X(41) VALUE
                  '1732PITUBA               15.142.490017375'.
               05 FILLER               PIC X(41) VALUE
                  '1759NOVA VICOSA          15.142.490017537'.
               05 FILLER               PIC X(41) VALUE
                  '1767UBAITABA             15.142.490017618'.
               05 FILLER               PIC X(41) VALUE
                  '1775SERRA PRETA          15.142.490017707'.
               05 FILLER               PIC X(41) VALUE
                  '1791BROTAS DE MACAUBA    15.142.490017960'.
               05 FILLER               PIC X(41) VALUE
                  '1821PEDRAO               15.142.490018266'.
               05 FILLER               PIC X(41) VALUE
                  '1856ITABELA              15.142.490018509'.
               05 FILLER               PIC X(41) VALUE
                  '1872MORPARA              15.142.490018770'.
               05 FILLER               PIC X(41) VALUE
                  '1880PAULO AFONSO         15.142.490018851'.
               05 FILLER               PIC X(41) VALUE
                  '1899IPECAETA             15.142.490018932'.
               05 FILLER               PIC X(41) VALUE
                  '1945PALMAS DO M. ALTO    15.142.490019408'.
               05 FILLER               PIC X(41) VALUE
                  '1953CAMPO FORMOSO        15.142.490019580'.
               05 FILLER               PIC X(41) VALUE
                  '1970COARACI              15.142.490019742'.
               05 FILLER               PIC X(41) VALUE
                  '1988ICHU                 15.142.490019823'.
               05 FILLER               PIC X(41) VALUE
                  '1996CAATIBA              15.142.490019904'.
               05 FILLER               PIC X(41) VALUE
                  '2003IBICOARA             15.142.490020082'.
               05 FILLER               PIC X(41) VALUE
                  '2011IBIPEBA              15.142.490020163'.
               05 FILLER               PIC X(41) VALUE
                  '2062JANDAIRA             15.142.490020678'.
               05 FILLER               PIC X(41) VALUE
                  '2070SIMOES FILHO         15.142.490020759'.
               05 FILLER               PIC X(41) VALUE
                  '2100SANTA BRIGIDA        15.142.490021054'.
               05 FILLER               PIC X(41) VALUE
                  '2119ANTONIO GONCALVES    15.142.490021135'.
               05 FILLER               PIC X(41) VALUE
                  '2127IBIPITANGA           15.142.490021216'.
               05 FILLER               PIC X(41) VALUE
                  '2143CONCEICAO DO COITE   15.142.490021488'.
               05 FILLER               PIC X(41) VALUE
                  '2161QUIJINQUE            15.142.490021669'.
               05 FILLER               PIC X(41) VALUE
                  '2178STA CRUZ DA VITORIA  15.142.490021720'.
               05 FILLER               PIC X(41) VALUE
                  '6324CRISOPOLIS           15.142.490063298'.
               05 FILLER               PIC X(41) VALUE
                  '2232IRARA                15.142.490022379'.
               05 FILLER               PIC X(41) VALUE
                  '2240OURICANGAS           15.142.490022450'.
               05 FILLER               PIC X(41) VALUE
                  '2259CANAVIEIRAS          15.142.490022630'.
               05 FILLER               PIC X(41) VALUE
                  '2267PORTO SECO PIRAJA    15.142.490022611'.
               05 FILLER               PIC X(41) VALUE
                  '2283MALHADA DE PEDRAS    15.142.490022883'.
               05 FILLER               PIC X(41) VALUE
                  '2299ITAJUIPE             15.142.490022964'.
               05 FILLER               PIC X(41) VALUE
                  '2305CHORROCHO            15.142.490023006'.
               05 FILLER               PIC X(41) VALUE
                  '2313REMANSO              15.142.490023189'.
               05 FILLER               PIC X(41) VALUE
                  '2330IPIRA                15.142.490023340'.
               05 FILLER               PIC X(41) VALUE
                  '2356GRACA                15.142.490023502'.
               05 FILLER               PIC X(41) VALUE
                  '2364EUNAPOLIS            15.142.490023693'.
               05 FILLER               PIC X(41) VALUE
                  '2372CRUZ DAS ALMAS       15.142.490023774'.
               05 FILLER               PIC X(41) VALUE
                  '2380SANTO AMARO          15.142.490023855'.
               05 FILLER               PIC X(41) VALUE
                  '2399SERRINHA             15.142.490023936'.
               05 FILLER               PIC X(41) VALUE
                  '2402BAIRRO BRASIL        15.142.490024070'.
               05 FILLER               PIC X(41) VALUE
                  '2410IBOTIRAMA            15.142.490024150'.
               05 FILLER               PIC X(41) VALUE
                  '2429BASE                 15.142.490024231'.
               05 FILLER               PIC X(41) VALUE
                  '2437SERVIDOR             15.142.490024312'.
               05 FILLER               PIC X(41) VALUE
                  '2445SENHOR DOS PASSOS    15.142.490024401'.
               05 FILLER               PIC X(41) VALUE
                  '2453PELOURINHO           15.142.490024504'.
               05 FILLER               PIC X(41) VALUE
                  '2461SUMARE               15.142.490024665'.
               05 FILLER               PIC X(41) VALUE
                  '2470PERIPERI             15.142.490024746'.
               05 FILLER               PIC X(41) VALUE
                  '2488ITAPUA               15.142.490024827'.
               05 FILLER               PIC X(41) VALUE
                  '2496LITORAL NORTE        15.142.490024908'.
               05 FILLER               PIC X(41) VALUE
                  '2500LUIZ E. MAGALHAES    15.142.490025041'.
               05 FILLER               PIC X(41) VALUE
                  '2518RIO VERMELHO         15.142.490025122'.
               05 FILLER               PIC X(41) VALUE
                  '2526SHOPPING BARRA       15.142.490025203'.
               05 FILLER               PIC X(41) VALUE
                  '2534UNIQ                 15.142.490025394'.
               05 FILLER               PIC X(41) VALUE
                  '2542FAZENDA GRANDE       15.142.490025475'.
           03 WRK-TAB-INTERNA REDEFINES  WRK-TABELA
                          OCCURS 169 TIMES INDEXED BY WRK-IND.
               05 WRK-TB-COD       PIC 9(03).
               05 WRK-TB-DIG       PIC 9(01).
               05 WRK-TB-NOME      PIC X(21).
               05 WRK-TB-CGC       PIC 9(10).
               05 WRK-TB-FILIAL    PIC 9(04).
               05 WRK-TB-CTL       PIC 9(02).

NAV506*----------------------------------------------------------------*
NAV506*01  FILLER                      PIC  X(032)         VALUE
NAV506*    '*** AREA DO RUEC8870         ***'.
NAV506*----------------------------------------------------------------*
NAV506*01  WRK-AREA-8870.
NAV506*    05 WRK-ENTRADA.
NAV506*       10 WRK-PGM-CHAMADOR            PIC  X(008) VALUE SPACES.
NAV506*       10 WRK-AGENCIA8870             PIC  9(005) VALUE ZEROS.
NAV506*       10 WRK-CEDULA                  PIC  9(009) VALUE ZEROS.
NAV506*       10 WRK-TIPO-PESSOA             PIC  X(001) VALUE SPACES.
NAV506*       10 WRK-IDENTIF                 PIC  9(015) VALUE ZEROS.
NAV506*       10 WRK-CGC                     REDEFINES WRK-IDENTIF.
NAV506*           15  WRK-NUMCGC             PIC 9(09).
NAV506*           15  WRK-FILIAL             PIC 9(04).
NAV506*           15  WRK-CTRL-CGC           PIC 9(02).
NAV506*       10  WRK-CPF                    REDEFINES WRK-IDENTIF.
NAV506*           15  WRK-NUMCPF             PIC 9(09).
NAV506*           15  WRK-CTRL-CPF           PIC 9(02).
NAV506*           15  WRK-FILLER             PIC 9(04).
NAV506*       10 WRK-NUMREFORIG-X.
NAV506*           15 WRK-NUMREFORIG          PIC 9(11)  VALUE ZEROS.
NAV506*       10 FILLER                      PIC X(055) VALUE SPACES.
NAV506*    05 WRK-RETORNO.
NAV506*       10 WRK-COD-RET                 PIC  9(002) VALUE ZEROS.
NAV506*
NAV506*=======  00 - OK
NAV506*=======  01 - DADOS INCONSISTENTES
NAV506*=======  99 - ERRO DB2
NAV506*       10 WRK-MENSAGEM8870            PIC  X(079) VALUE SPACES.
NAV506*       10 WRK-CONTROLE-BACEN          PIC  X(020) VALUE SPACES.
NAV506*       10 FILLER                      PIC  X(030) VALUE SPACES.
NAV506*       10 WRK-SQLCA                   PIC  X(136) VALUE SPACES.
NAV506*       10 WRK-ERRO-AREA               PIC  X(107) VALUE SPACES.
NAV506*
NAV506*----------------------------------------------------------------*
NAV506*01  FILLER                      PIC  X(032)         VALUE
NAV506*    '*** AREA DO RUCA8871         ***'.
NAV506*----------------------------------------------------------------*
NAV506*01  WRK-AREA-8871.
NAV506*    05 WRK-ENTRADA-8871.
NAV506*       10 WRK-PGM-CHAMADOR-8871       PIC  X(008) VALUE SPACES.
NAV506*       10 WRK-AGENCIA-8871            PIC  9(005) VALUE ZEROS.
NAV506*       10 WRK-CEDULA-8871             PIC  9(009) VALUE ZEROS.
NAV506*       10 WRK-FUNCAO-8871             PIC  X(001) VALUE SPACES.
NAV506*       10 WRK-TP-DESCLASS             PIC  X(001) VALUE SPACES.
NAV506*       10 WRK-ULT-CHAMADA             PIC  X(001) VALUE SPACES.
NAV506*       10 WRK-NUM-CTRL-IF             PIC  X(020) VALUE SPACES.
NAV506*       10 WRK-NUM-SEQ-DESCLA          PIC  9(003) VALUE ZEROS.
NAV506*       10 WRK-VLRTOTOP            PIC  9(14)V9(02) VALUE ZEROS.
NAV506*       10 WRK-VLRPRINCIPALPARCL   PIC  9(14)V9(02) VALUE ZEROS.
NAV506*       10 WRK-MSGEM-COR0003-TEXT      PIC  X(32500) VALUE SPACES.
NAV506*       10 FILLER                      PIC  X(100) VALUE SPACES.
NAV506*    05 WRK-RETORNO-8871.
NAV506*       10 WRK-COD-RET-8871            PIC  9(002) VALUE ZEROS.
NAV506*=======  00 - OK
NAV506*=======  01 - DADOS INCONSISTENTES
NAV506*=======  99 - ERRO DB2
NAV506*       10 WRK-MENSAGEM-8871           PIC  X(079) VALUE SPACES.
NAV506*       10 WRK-CONTROLE-BACEN-COR0003  PIC  X(020) VALUE SPACES.
NAV506*       10 WRK-CONTROLE-BACEN-COR0001  PIC  X(020) VALUE SPACES.
NAV506*       10 FILLER                      PIC  X(030) VALUE SPACES.
NAV506*       10 WRK-SQLCA-8871              PIC  X(136) VALUE SPACES.
NAV506*       10 WRK-ERRO-AREA-8871          PIC  X(107) VALUE SPACES.

WP1122*---------------------------------------------------------------*
-      01  FILLER                      PIC X(079)          VALUE
-          '*** AREA PARA CHAMADA MODULO RUEC8831 ***'.
-     *---------------------------------------------------------------*
-
-      COPY 'I#RUEC65'.
WP1122
      *--- AREA POOL7100 ----------------------------------------------*

       COPY 'POL7100C'.
      *--- AREA DE NULIDADE DAS TABELAS -------------------------------*

       01  WRK-RUFIV002-NULL.
           03 WRK-CGARTD-NULL                PIC S9(04) COMP VALUE +0.

      *--- AREA SQL ---------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIV002
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB003
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB044
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB097
           END-EXEC.

NAV506*   -EXEC SQL
NAV506*       -INCLUDE RUECB09C
NAV506*   -END-EXEC.
NAV506*
NAV506*----------------------------------------------------------------*
NAV506*01  FILLER                      PIC  X(032)         VALUE
NAV506*    '*** AREA DE CURSORES         ***'.
NAV506*----------------------------------------------------------------*
NAV506*
NAV506*   -EXEC SQL
NAV506*        -DECLARE CSR01-RUECB09C CURSOR FOR
NAV506*         -SELECT   WCONTD_MSGEM_RURAL
NAV506*         -FROM DB2PRD.TMNTRC_MSGEM_RURAL
NAV506*        -WHERE NMNTRC_MSGEM_RURAL >  0
NAV506*          -AND CMSGEM_RECOR       =  'COR0003R1'
NAV506*          -AND CCTRL_RQUIS_FINCR  =  :RUECB09C.CCTRL-RQUIS-FINCR
NAV506*   -END-EXEC.
NAV506*
       01  FILLER                      PIC  X(46)        VALUE
           '*** F I M  DA WORKING DO PROGRAMA RUCA6651 ***'.

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           03  LNK-IO-LTERM            PIC  X(08).
           03  FILLER                  PIC  X(02).
           03  LNK-IO-STATUS           PIC  X(02).
           03  FILLER                  PIC  X(12).
           03  LNK-IO-MODNAME          PIC  X(08).

       01  LNK-ALT-PCB.
           03  LNK-ALT-LTERM           PIC  X(08).
           03  FILLER                  PIC  X(02).
           03  LNK-ALT-STATUS          PIC  X(02).
           03  FILLER                  PIC  X(12).
           03  LNK-ALT-MODNAME         PIC  X(08).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

           EXEC SQL
           SET :WRK-TIMESTAMP = CURRENT TIMESTAMP
           END-EXEC.

      *---------------------------------------------------------------*
       0000-OBTER-MENSAGEM             SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL5000'             USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-NOVA-OPCAO
                                             WRK-COD-USER
                                             WRK-OPCAO-ATUAL
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               GOBACK
           END-IF.

           MOVE 'RUCAT651'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'RUCA6651'
201609         IF WRK-TRANSACAO        EQUAL 'RURC2770'
201609            PERFORM 1001-TRATAR-RETORNO-PF9
201609         ELSE
                  INSPECT WRK-OUTPUT-T651 REPLACING ALL ZEROS BY SPACES
                  MOVE WRK-MSG001         TO WRK-OUT-MENSAG-T651
201609         END-IF
           ELSE
               PERFORM 2000-PROCESSAR-RUCA6651
           END-IF.

           IF  WRK-TELA                EQUAL 'RUCAT651'
               PERFORM 4000-POOL0660-RUCAT651
           END-IF.

           GO TO 0000-OBTER-MENSAGEM.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
201609*----------------------------------------------------------------*
201609 1001-TRATAR-RETORNO-PF9         SECTION.
201609*----------------------------------------------------------------*
201609
201609     MOVE WRK-MENSAGEM (1:LENGTH OF  WRK-2770-AREA)
201609                                 TO  WRK-2770-AREA.
201609     MOVE WRK-2770-NAVEGACAO     (1:242)
201609                                 TO  WRK-INPUT-T651
201609     MOVE '.'                    TO  WRK-INP-PFK-T651.
201609     MOVE WRK-2770-MOTIVO-SELCD-N
201609                                 TO  WRK-INP-MOTDES-T651.
201609     PERFORM 2001-DEVOLVER-TELA.
201609
201609*----------------------------------------------------------------*
201609 1001-99-FIM.                    EXIT.
201609*----------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-PROCESSAR-RUCA6651         SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-T651.
           PERFORM 2001-DEVOLVER-TELA.

BRQ001     IF (WRK-INP-FASE-T651       EQUAL '3' OR '4') AND
BRQ001        (WRK-INP-PFK-T651        EQUAL '.')
BRQ001        MOVE '6'                 TO WRK-INP-PFK-T651
BRQ001     END-IF.

           EVALUATE WRK-INP-PFK-T651

               WHEN  '.'
                   PERFORM 2100-TRATAR-ENTER

               WHEN  '6'
BRQ001               EVALUATE WRK-INP-FASE-T651
BRQ001                 WHEN '2'
BRQ001                   PERFORM 2214-VERIFICA-DESCLA-TOT
NAV506****              -IF WRK-TP-DESCLASS EQUAL 'T'
BRQ001                     PERFORM 2200-CONFIRMAR-INCLUSAO
WP1122                     PERFORM 2220-ENVIAR-COR0005
BRQ001                     PERFORM 2213-INCLUSAO-EFETUADA
NAV506****              -ELSE
NAV506****                -PERFORM 2151-OBTER-DADOS-RUFIV002
NAV506****                -PERFORM 2205-ENVIAR-COR0003
NAV506****              -END-IF
NAV506****            -WHEN '3'
BRQ001*----BUSCA-WCONTD-NA-TABELA-RUECB09C-COM-CTRLIF-E-COR0003R1
NAV506****              -PERFORM 2206-COUNT-RUECB09C
NAV506****              -IF WRK-TOTAL-COR0003R1 EQUAL ZEROS
NAV506****                  -MOVE 'AGUARDANDO RETORNO BACEN - COR0003'
NAV506****                                  TO WRK-OUT-MENSAG-T651
NAV506****              -ELSE
NAV506****                  -IF ACU-ENVIO-COR0001 EQUAL ZEROES
NAV506****                     -PERFORM 2200-CONFIRMAR-INCLUSAO
NAV506****                  -END-IF
NAV506****                  -PERFORM 2207-OPEN-COR0003R1
NAV506****                  -PERFORM 2208-FETCH-COR0003R1 UNTIL
NAV506****                                 SQLCODE EQUAL +100
NAV506****                  -PERFORM 2209-CLOSE-COR0003R1
NAV506****                  -IF ACU-ENVIO-COR0001 GREATER ZEROS
NAV506****                     -MOVE '4'       TO WRK-OUT-FASE-T651
NAV506****                     -MOVE 'COR0001 ENVIADA BACEN'
NAV506****                                  TO WRK-OUT-MENSAG-T651
NAV506****                  -END-IF
NAV506****              -END-IF
NAV506****            -WHEN '4'
BRQ001*----BUSCA-WCONTD-NA-TABELA-RUECB09C-COM-CTRLIF-E-COR0001R1
NAV506****              -PERFORM 2211-SELECT-COR0001R1
BRQ001               END-EVALUATE
301609         WHEN  '9'
201609             PERFORM 2340-PASSAR-CONTROLE-RURC2770
201609
               WHEN  'A'
                   PERFORM 2300-TRATAR-PF3

               WHEN OTHER
                   MOVE WRK-MSG002     TO WRK-OUT-MENSAG-T651

           END-EVALUATE.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2001-DEVOLVER-TELA              SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-INP-FASE-T651      TO WRK-OUT-FASE-T651.
           MOVE WRK-INP-NROPER-T651-X  TO WRK-OUT-NROPER-T651-X.
           MOVE WRK-INP-DIGOPER-T651   TO WRK-OUT-DIGOPER-T651.
           MOVE WRK-INP-AGENCIA-T651-X TO WRK-OUT-AGENCIA-T651-X.
           MOVE WRK-INP-NOMAG-T651     TO WRK-OUT-NOMAG-T651.
           MOVE WRK-INP-AAFINAN-T651-X TO WRK-OUT-AAFINAN-T651-X.
           MOVE WRK-INP-NOME-T651      TO WRK-OUT-NOME-T651.
           MOVE WRK-INP-CC-T651-X      TO WRK-OUT-CC-T651-X.
           MOVE WRK-INP-CCDIG-T651-X   TO WRK-OUT-CCDIG-T651-X.
           MOVE WRK-INP-CGCR-T651-X    TO WRK-OUT-CGCR-T651-X
           MOVE WRK-INP-CGCF-T651-X    TO WRK-OUT-CGCF-T651-X.
           MOVE WRK-INP-CGCD-T651-X    TO WRK-OUT-CGCD-T651-X.
           MOVE WRK-INP-CREDITO-T651   TO WRK-OUT-CREDITO-T651-X.
           MOVE WRK-INP-DEVEDOR-T651   TO WRK-OUT-DEVEDOR-T651-X.
           MOVE WRK-INP-VLRDESC-T651   TO WRK-OUT-VLRDESC-T651-X.
           MOVE WRK-INP-DTDESC-T651    TO WRK-OUT-DTDESC-T651.
           MOVE WRK-INP-MOTDES-T651-X  TO WRK-OUT-MOTDES-T651-X.
           MOVE WRK-INP-MOTDES1-T651   TO WRK-OUT-MOTDES1-T651.
BRQ001     MOVE WRK-INP-CTRLIF-T651    TO WRK-OUT-CTRLIF-T651.
BRQ001     MOVE WRK-INP-NMOTVO-T651    TO WRK-OUT-NMOTVO-T651.

      *---------------------------------------------------------------*
       2001-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2100-TRATAR-ENTER               SECTION.
      *---------------------------------------------------------------*

           IF  WRK-INP-FASE-T651       EQUAL '2'
               MOVE '1'                TO WRK-OUT-FASE-T651
               MOVE 'INFORME A CHAVE DA OPERACAO E TECLE ENTER'
                                       TO WRK-OUT-MENSAG-T651
               MOVE WRK-49361          TO WRK-OUT-OPER-ATTR-T651
                                          WRK-OUT-VLRDESC-ATTR-T651
                                          WRK-OUT-DTDESC-ATTR-T651
                                          WRK-OUT-MOTDES-ATTR-T651
               GO TO 2100-99-FIM
           END-IF.

           MOVE 'N'                    TO WRK-INCONSISTENCIA

           PERFORM 2101-CONSISTIR-ENTRADA.

           IF  WRK-INCONSISTENCIA      NOT EQUAL 'S'
               MOVE '2'                TO WRK-OUT-FASE-T651
               MOVE 'TECLE PF6 PARA CONFIRMAR OU ENTER PARA ALTERAR'
                                       TO WRK-OUT-MENSAG-T651
               MOVE WRK-225            TO WRK-OUT-OPER-ATTR-T651
                                          WRK-OUT-VLRDESC-ATTR-T651
                                          WRK-OUT-DTDESC-ATTR-T651
                                          WRK-OUT-MOTDES-ATTR-T651
           END-IF.

      *---------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2101-CONSISTIR-ENTRADA          SECTION.
      *---------------------------------------------------------------*

           PERFORM 2110-CONSISTIR-CHAVE-OPERACAO.

           IF  WRK-INCONSISTENCIA      EQUAL 'S'
               GO TO 2101-99-FIM
           END-IF.

           PERFORM 2120-CONSISTIR-VALOR-DESCL.

           IF  WRK-INCONSISTENCIA      EQUAL 'S'
               GO TO 2101-99-FIM
           END-IF.

           PERFORM 2130-CONSISTIR-DATA-DESCL.

           IF  WRK-INCONSISTENCIA      EQUAL 'S'
               GO TO 2101-99-FIM
           END-IF.

           PERFORM 2140-CONSISTIR-MOTIVO-DESCL.


      *---------------------------------------------------------------*
       2101-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2110-CONSISTIR-CHAVE-OPERACAO   SECTION.
      *---------------------------------------------------------------*

           IF  WRK-INP-NROPER-T651-X   EQUAL SPACES OR LOW-VALUES
               MOVE 'INFORME A CHAVE DA OPERACAO'
                                       TO WRK-OUT-MENSAG-T651
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-OPER-ATTR-T651
           END-IF.

           INSPECT WRK-INP-NROPER-T651-X REPLACING LEADING ' ' BY '0'.

           IF  WRK-INP-NROPER-T651-X   NOT NUMERIC
               MOVE 'CHAVE DA OPERACAO INVALIDA'
                                       TO WRK-OUT-MENSAG-T651
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-OPER-ATTR-T651
           END-IF.

           IF  WRK-INCONSISTENCIA      NOT EQUAL 'S'
               PERFORM 2150-OBTER-DIGOPER
               PERFORM 2151-OBTER-DADOS-RUFIV002
               IF  WRK-NAO-ENCONTRADO  EQUAL 'S'
                   MOVE 'S'            TO WRK-INCONSISTENCIA
                   MOVE 'OPERACAO INFORMADA NAO CADASTRADA'
                                       TO WRK-OUT-MENSAG-T651
                   MOVE WRK-49369      TO WRK-OUT-OPER-ATTR-T651
                   GO TO 2110-99-FIM
               END-IF
               PERFORM 2152-OBTER-DADOS-RUCAB003
BRQ999         IF  SQLCODE             EQUAL +100
BRQ999             MOVE 'S'            TO WRK-INCONSISTENCIA
BRQ999             MOVE 'OPERACAO INFORMADA NAO CADASTRADA NA RUCAB003'
BRQ999                                 TO WRK-OUT-MENSAG-T651
BRQ999             MOVE WRK-49369      TO WRK-OUT-OPER-ATTR-T651
BRQ999             GO TO 2110-99-FIM
BRQ999         END-IF
               PERFORM 2153-MONTAR-DADOS-SAIDA
           END-IF.

      *---------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2120-CONSISTIR-VALOR-DESCL      SECTION.
      *---------------------------------------------------------------*

           IF  WRK-INP-VLRDESC-T651    EQUAL SPACES
           OR  WRK-INP-VLRDESC-T651    EQUAL LOW-VALUES
               MOVE 'INFORME O VALOR A DESCLASSIFICAR'
                                       TO WRK-OUT-MENSAG-T651
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-VLRDESC-ATTR-T651
               GO TO 2120-99-FIM
           END-IF.

           MOVE 14                     TO WRK-POOL0496-TAM-INT.
           MOVE 02                     TO WRK-POOL0496-TAM-DEC.
           MOVE WRK-INP-VLRDESC-T651   TO WRK-POOL0496-ENTRADA.
           MOVE ZEROS                  TO RETURN-CODE
                                          WRK-POOL0496-RETURN-CODE.
           MOVE SPACES                 TO WRK-POOL0496-SAIDA.

           CALL 'POOL0496'             USING WRK-POOL0496-ENTRADA
                                             WRK-POOL0496-SAIDA
                                             WRK-POOL0496-TAMANHO.

           MOVE RETURN-CODE            TO WRK-POOL0496-RETURN-CODE.

           IF  WRK-POOL0496-RETURN-CODE EQUAL ZEROS OR 20
               MOVE WRK-POOL0496-SAIDA TO WRK-CH21
               MOVE WRK-ZD14V2         TO WRK-OUT-VLRDESC-T651
                                          WRK-VALOR-DESCL
           ELSE
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-VLRDESC-ATTR-T651
               MOVE 'VALOR A DESCLASSIFICAR INVALIDO'
                                       TO WRK-OUT-MENSAG-T651
           END-IF.

      *---------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2130-CONSISTIR-DATA-DESCL       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-AREA-POOL1205.
           MOVE SPACES                 TO WRK-1205-MENSAGEM.

           IF  WRK-INP-DTDESC-T651     EQUAL SPACES OR LOW-VALUES
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE 'INFORME A DATA DA DESCLASSIFICACAO NO FORMATO DD/MM
      -              '/AAAA OU DDMMAAAA'
                                       TO WRK-OUT-MENSAG-T651
               MOVE WRK-49369          TO WRK-OUT-DTDESC-ATTR-T651
              GO TO 2130-99-FIM
           END-IF.

           MOVE WRK-INP-DTDESC-T651(7:4)
                                       TO WRK-ANO-ISO
           IF  WRK-ANO-ISO             NOT NUMERIC
               MOVE 'DATA DE DESCLASSIFICACAO INVALIDA - ANO NAO NUMERIC
      -              'O'               TO WRK-OUT-MENSAG-T651
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-DTDESC-ATTR-T651
               GO TO 2130-99-FIM
           END-IF.

           IF  WRK-INP-DTDESC-T651(6:1)        NOT NUMERIC
               MOVE WRK-INP-DTDESC-T651(4:2)   TO WRK-MES-ISO
               MOVE WRK-INP-DTDESC-T651(1:2)   TO WRK-DIA-ISO
           ELSE
               IF WRK-INP-DTDESC-T651(1:2)     NOT EQUAL SPACES AND
                                                         LOW-VALUES
                  MOVE 'DATA DE DESCLASSIFICACAO INVALIDA'
                                       TO WRK-OUT-MENSAG-T651
                  MOVE 'S'             TO WRK-INCONSISTENCIA
                  MOVE WRK-49369       TO WRK-OUT-DTDESC-ATTR-T651
                  GO TO 2130-99-FIM
               ELSE
                  MOVE WRK-INP-DTDESC-T651(5:2) TO WRK-MES-ISO
                  MOVE WRK-INP-DTDESC-T651(3:2) TO WRK-DIA-ISO
               END-IF
           END-IF

           IF  WRK-MES-ISO             NOT NUMERIC
               MOVE 'DATA DE DESCLASSIFICACAO INVALIDA - MES NAO NUMERIC
      -             'O'                TO WRK-OUT-MENSAG-T651
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-DTDESC-ATTR-T651
               GO TO 2130-99-FIM
           END-IF.

           IF  WRK-DIA-ISO             NOT NUMERIC
               MOVE 'DATA DE DESCLASSIFICACAO INVALIDA - DIA NAO NUMERIC
      -             'O'                TO WRK-OUT-MENSAG-T651
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-DTDESC-ATTR-T651
               GO TO 2130-99-FIM
           END-IF.

           MOVE WRK-DATA-ISO           TO WRK-1205-DATA

           CALL 'POOL1205'             USING WRK-AREA-POOL1205
                                             WRK-1205-MENSAGEM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'DATA DE DESCLASSIFICACAO INVALIDA'
                                       TO WRK-OUT-MENSAG-T651
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-DTDESC-ATTR-T651
               GO TO 2130-99-FIM
           END-IF.

           MOVE WRK-DIA-ISO            TO WRK-OUT-DTDESC-T651(1:2).
           MOVE '/'                    TO WRK-OUT-DTDESC-T651(3:1).
           MOVE WRK-MES-ISO            TO WRK-OUT-DTDESC-T651(4:2).
           MOVE '/'                    TO WRK-OUT-DTDESC-T651(6:1).
           MOVE WRK-ANO-ISO            TO WRK-OUT-DTDESC-T651(7:4).
           MOVE WRK-OUT-DTDESC-T651    TO WRK-DATA-DESCL.
           INSPECT WRK-DATA-DESCL REPLACING ALL '/' BY '.'.

      *---------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2140-CONSISTIR-MOTIVO-DESCL     SECTION.
      *---------------------------------------------------------------*

           IF  WRK-INP-MOTDES-T651-X   EQUAL SPACES OR
               WRK-INP-MOTDES-T651     EQUAL ZEROS
               MOVE '1'                TO WRK-OUT-FASE-T651
               MOVE 'INFORME O MOTIVO DA DESCLASSIFICACAO'
                                       TO WRK-OUT-MENSAG-T651
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-MOTDES-ATTR-T651
               GO TO 2140-99-FIM
           END-IF.

           INSPECT WRK-INP-MOTDES-T651-X REPLACING LEADING ' ' BY '0'.
           IF  WRK-INP-MOTDES-T651-X   NOT NUMERIC
               MOVE '1'                TO WRK-OUT-FASE-T651
               MOVE 'MOTIVO DA DESCLASSIFICACAO DEVE SER NUMERICO'
                                       TO WRK-OUT-MENSAG-T651
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-MOTDES-ATTR-T651
               GO TO 2140-99-FIM
           END-IF.

           PERFORM 2157-DESCRICAO-MOTIVO.

           IF  SQLCODE                 EQUAL +100
               MOVE '1'                TO WRK-OUT-FASE-T651
               MOVE 'MOTIVO DA DESCLASSIFICACAO NAO CADASTRADO'
                                       TO WRK-OUT-MENSAG-T651
               MOVE 'S'                TO WRK-INCONSISTENCIA
               MOVE WRK-49369          TO WRK-OUT-MOTDES-ATTR-T651
           ELSE
               MOVE RMOTVO-DCLAS-OPER  OF RURCB097
                                       TO WRK-OUT-MOTDES1-T651
           END-IF.

      *---------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2150-OBTER-DIGOPER              SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-AREA-POOL0432.

           MOVE WRK-INP-NROPER-T651    TO WRK-VALOR-0432.
           MOVE 9                      TO WRK-TAMANHO-0432.

           CALL 'POOL0432'             USING WRK-VALOR-0432
                                             WRK-DIGITO-0432
                                             WRK-TAMANHO-0432.

           EVALUATE WRK-DIGITO-0432

              WHEN '.'
                 MOVE 'APL'            TO ERR-TIPO-ACESSO
                 MOVE 'ERRO NO ACESSO A POOL0432'
                                       TO ERR-TEXTO
                 PERFORM 9999-ROTINA-ERRO

              WHEN 'P'
                 MOVE ZEROS            TO WRK-DIGOPER

              WHEN OTHER
                MOVE WRK-DIGITO-0432   TO WRK-DIGOPER

           END-EVALUATE.

      *---------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2151-OBTER-DADOS-RUFIV002       SECTION.
      *---------------------------------------------------------------*

           MOVE 'N'                    TO WRK-NAO-ENCONTRADO.
           MOVE WRK-INP-NROPER-T651    TO CCONTR      OF    RUFIV002.

           EXEC SQL
             SELECT
               CJUNC_DEPDC
              ,CCTA_CORR
              ,VFINAN
              ,CCDULA
              ,CGARTD
BRQ001        ,CREFT_RECOR
             INTO
               :RUFIV002.CJUNC-DEPDC
              ,:RUFIV002.CCTA-CORR
              ,:RUFIV002.VFINAN
              ,:RUFIV002.CCDULA
              ,:RUFIV002.CGARTD
               :WRK-CGARTD-NULL
BRQ001        ,:RUFIV002.CREFT-RECOR
BRQ001         :WRK-CREFT-RECOR-NULL
             FROM DB2PRD.V01CONTRATO_RURAL
             WHERE
               CCONTR      = :RUFIV002.CCONTR
           END-EXEC.

           IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
              ( SQLWARN0               EQUAL 'W' )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'V01CONTRATO_RURAL ' TO ERR-DBD-TAB
                MOVE 'SELECT  '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '0001'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL +100
               MOVE 'S'                   TO WRK-NAO-ENCONTRADO
           END-IF.

           IF  WRK-CGARTD-NULL           LESS ZEROS
               MOVE ZEROS                 TO CGARTD      OF RUFIV002
           END-IF.

      *---------------------------------------------------------------*
       2151-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2152-OBTER-DADOS-RUCAB003       SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-INP-NROPER-T651    TO B003-CH-CONTR OF  RUCAB003.

           EXEC SQL
             SELECT
               B003_NOMUTUAR1
              ,B003_CGCPF1
              ,B003_ORIGREC1
              ,B003_ORIGREC2
              ,B003_ORIGREC3
              ,B003_ORIGREC4
              ,B003_JURPGAC_PAG8
              ,B003_MEDIPGAC_PAG8
              ,B003_CORRPGAC_PAG8
              ,B003_JURPGAC_PAG9
              ,B003_VLPRINAC_PAG9
              ,B003_CORRPGAC_PAG9
              ,B003_VLPRINCT_CAP9
              ,B003_JURPGORI_CAP9
              ,B003_CORRPGOR_CAP9
              ,B003_VLAUXIL_OPER9
             INTO
               :RUCAB003.B003-NOMUTUAR1
              ,:RUCAB003.B003-CGCPF1
              ,:RUCAB003.B003-ORIGREC1
              ,:RUCAB003.B003-ORIGREC2
              ,:RUCAB003.B003-ORIGREC3
              ,:RUCAB003.B003-ORIGREC4
              ,:RUCAB003.B003-JURPGAC-PAG8
              ,:RUCAB003.B003-MEDIPGAC-PAG8
              ,:RUCAB003.B003-CORRPGAC-PAG8
              ,:RUCAB003.B003-JURPGAC-PAG9
              ,:RUCAB003.B003-VLPRINAC-PAG9
              ,:RUCAB003.B003-CORRPGAC-PAG9
              ,:RUCAB003.B003-VLPRINCT-CAP9
              ,:RUCAB003.B003-JURPGORI-CAP9
              ,:RUCAB003.B003-CORRPGOR-CAP9
              ,:RUCAB003.B003-VLAUXIL-OPER9
             FROM DB2PRD.RUCA_CAD_ROOT
             WHERE
               B003_CH_CONTR     = :RUCAB003.B003-CH-CONTR
           END-EXEC

           IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
              ( SQLWARN0               EQUAL 'W' )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'RUCA_CAD_ROOT'      TO ERR-DBD-TAB
                MOVE 'SELECT  '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '0002'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2152-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2153-MONTAR-DADOS-SAIDA         SECTION.
      *---------------------------------------------------------------*

           PERFORM 2154-OBTER-NOME-AGENCIA.

           MOVE WRK-INP-NROPER-T651          TO WRK-OUT-NROPER-T651.
           MOVE WRK-DIGOPER                  TO WRK-OUT-DIGOPER-T651.
           MOVE CJUNC-DEPDC    OF RUFIV002   TO WRK-OUT-AGENCIA-T651.
           MOVE WRK-NOME-AGENCIA             TO WRK-OUT-NOMAG-T651.
           MOVE CCDULA         OF RUFIV002   TO WRK-OUT-AAFINAN-T651.
           MOVE B003-NOMUTUAR1 OF RUCAB003   TO WRK-OUT-NOME-T651.
           MOVE CCTA-CORR      OF RUFIV002   TO WRK-OUT-CC-T651.

           MOVE  SPACES                      TO WRK-DIGITO-COSA.
           MOVE  '7'                         TO WRK-DIGITO-COSA-1.

           CALL  'POOL0431'               USING WRK-OUT-CC-T651
                                                WRK-DIGITO-COSA-1
                                                WRK-TAMANHO-0431

           IF    WRK-DIGITO-COSA-1        EQUAL '.'
                 MOVE SPACES                 TO WRK-OUT-CCDIG-T651-X
           ELSE
                 MOVE WRK-DIGITO-COSA        TO WRK-OUT-CCDIG-T651-X
           END-IF.

           MOVE B003-CGCPF1    OF RUCAB003   TO WRK-DESC-CGC-R.
           MOVE WRK-RAIZ-CGC                 TO WRK-OUT-CGCR-T651.
           MOVE WRK-FILIAL-CGC               TO WRK-OUT-CGCF-T651.
           MOVE WRK-DIG-CGC                  TO WRK-OUT-CGCD-T651.
           MOVE VFINAN         OF RUFIV002   TO WRK-OUT-CREDITO-T651.

           MOVE ZEROS                        TO WRK-ORIGEM-RECURSOS
                                                WRK-DEVEDOR.

           EVALUATE TRUE
           WHEN B003-ORIGREC4       OF RUCAB003 NOT EQUAL ZEROS
                MOVE B003-ORIGREC4  OF RUCAB003 TO WRK-ORIGEM-RECURSOS
           WHEN B003-ORIGREC3       OF RUCAB003 NOT EQUAL ZEROS
                MOVE B003-ORIGREC3  OF RUCAB003 TO WRK-ORIGEM-RECURSOS
           WHEN B003-ORIGREC2       OF RUCAB003 NOT EQUAL ZEROS
                MOVE B003-ORIGREC2  OF RUCAB003 TO WRK-ORIGEM-RECURSOS
           WHEN B003-ORIGREC1       OF RUCAB003 NOT EQUAL ZEROS
                MOVE B003-ORIGREC1  OF RUCAB003 TO WRK-ORIGEM-RECURSOS
           END-EVALUATE

           IF (WRK-ORIGEM-RECURSOS  GREATER 249   AND
               WRK-ORIGEM-RECURSOS  LESS    271 ) OR
              (WRK-ORIGEM-RECURSOS  GREATER 899   AND
               WRK-ORIGEM-RECURSOS  LESS    951 )
               COMPUTE WRK-DEVEDOR  =
               (B003-JURPGAC-PAG8   OF RUCAB003 -
                B003-JURPGAC-PAG9   OF RUCAB003) +
               (B003-MEDIPGAC-PAG8  OF RUCAB003 -
                B003-CORRPGAC-PAG9  OF RUCAB003) +
               (B003-VLPRINCT-CAP9  OF RUCAB003 -
                B003-VLPRINAC-PAG9  OF RUCAB003)
           ELSE
               COMPUTE WRK-DEVEDOR  =
               (B003-JURPGAC-PAG8   OF RUCAB003 -
                B003-JURPGORI-CAP9  OF RUCAB003) +
               (B003-CORRPGAC-PAG8  OF RUCAB003 -
                B003-CORRPGOR-CAP9  OF RUCAB003) +
                B003-VLAUXIL-OPER9  OF RUCAB003
           END-IF.

           MOVE WRK-DEVEDOR                  TO WRK-OUT-DEVEDOR-T651.

           INSPECT WRK-OUT-VLRDESC-T651 REPLACING ALL ZEROS BY SPACES.

           PERFORM 2157-DESCRICAO-MOTIVO.

           MOVE RMOTVO-DCLAS-OPER      OF RURCB097
                                       TO WRK-OUT-MOTDES1-T651.

      *---------------------------------------------------------------*
       2153-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2154-OBTER-NOME-AGENCIA         SECTION.
      *---------------------------------------------------------------*

           IF CGARTD    OF   RUFIV002  EQUAL 700
              PERFORM 2155-TAB-INTERNA
           ELSE
              PERFORM 2156-ACESSAR-MESU9032
           END-IF.

      *---------------------------------------------------------------*
       2154-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2155-TAB-INTERNA                SECTION.
      *---------------------------------------------------------------*

           SET WRK-IND TO 1.

           SEARCH  WRK-TAB-INTERNA
           WHEN WRK-TB-COD(WRK-IND)       EQUAL CJUNC-DEPDC OF RUFIV002
                MOVE WRK-TB-NOME(WRK-IND) TO WRK-NOME-AGENCIA
           END-SEARCH.

      *---------------------------------------------------------------*
       2155-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2156-ACESSAR-MESU9032           SECTION.
      *---------------------------------------------------------------*

           INITIALIZE  WRK-MESU9032
                       WRK-POOL7100-MESU9032
                       WRK-SQLCA-MESU9032
                       WRK-AMBIENTE-MESU9032
                       WRK-IO-PCB-MESU9032
                       WRK-ALT-PCB-MESU9032.

           MOVE CJUNC-DEPDC            OF RUFIV002
                                       TO WRK-AGENCIA.
           MOVE 'MESU9032'             TO WRK-MODULO.

           MOVE 'O'                    TO WRK-AMBIENTE-MESU9032.

           CALL WRK-MODULO          USING WRK-MESU9032
                                          WRK-POOL7100-MESU9032
                                          WRK-SQLCA-MESU9032
                                          WRK-AMBIENTE-MESU9032
                                          WRK-IO-PCB-MESU9032
                                          WRK-ALT-PCB-MESU9032.

           IF  WRK-COD-RETORNO     NOT  EQUAL  ZEROS
               MOVE 03                  TO WRK-RETURN-CODE
               MOVE 'AGENCIA INEXISTENTE NO MESU9032'
                                        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WRK-NOME             TO WRK-NOME-AGENCIA.

      *---------------------------------------------------------------*
       2156-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2157-DESCRICAO-MOTIVO           SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-INP-MOTDES-T651    TO CMOTVO-DCLAS-OPER OF RURCB097.

           EXEC SQL
             SELECT
               RMOTVO_DCLAS_OPER
             INTO
               :RURCB097.RMOTVO-DCLAS-OPER
             FROM  DB2PRD.TMOTVO_DCLAS_OPER
             WHERE CMOTVO_DCLAS_OPER   = :RURCB097.CMOTVO-DCLAS-OPER
           END-EXEC.

           IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
              ( SQLWARN0               EQUAL 'W' )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'TMOTVO_DCLAS_OPER ' TO ERR-DBD-TAB
                MOVE 'SELECT  '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '2108'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2157-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2200-CONFIRMAR-INCLUSAO         SECTION.
      *---------------------------------------------------------------*

BRQ001     IF  WRK-INP-FASE-T651       NOT EQUAL '2' AND '3'
               MOVE WRK-MSG002         TO WRK-OUT-MENSAG-T651
               GO TO 2200-99-FIM
           END-IF.

           MOVE 'N'                    TO WRK-INCONSISTENCIA

           PERFORM 2101-CONSISTIR-ENTRADA.

           IF  WRK-INCONSISTENCIA      EQUAL 'S'
               MOVE 'NAO FOI POSSIVEL INCLUIR'
                                       TO WRK-OUT-MENSAG-T651
               GO TO 2200-99-FIM
           END-IF.

           PERFORM 2202-MOVIMENTAR-CAMPOS.

           PERFORM 2203-SELECT-MAX-NMOTVO.

           PERFORM 2204-INSERIR-REGISTROS.

STE142     IF WRK-TP-DESCLASS EQUAL 'T'
STE142        PERFORM 2221-ALTERAR-RUFIV002
STE142
STE142        PERFORM 2222-ALTERAR-RUCAB003
STE142     END-IF.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2202-MOVIMENTAR-CAMPOS          SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-INP-NROPER-T651   TO CCONTR             OF RUFIB044.
           MOVE WRK-INP-MOTDES-T651   TO CMOTVO-DCLAS-OPER  OF RUFIB044.
           MOVE WRK-DATA-DESCL        TO DDCLAS-OPER-CREDT  OF RUFIB044.
           MOVE WRK-VALOR-DESCL       TO VPRINC-DCLAS-CONTR OF RUFIB044.
           MOVE WRK-COD-USER-X        TO CUSUAR-INCL        OF RUFIB044.
           MOVE SPACES                TO CUSUAR-MANUT       OF RUFIB044.
           MOVE -1                    TO WRK-CUSUAR-MANUT-NULL.
           MOVE SPACES                TO HMANUT-REG         OF RUFIB044.
           MOVE -1                    TO WRK-HMANUT-NULL.

      *---------------------------------------------------------------*
       2202-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2203-SELECT-MAX-NMOTVO          SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             SELECT
               IFNULL ( MAX ( NMOTVO_DCLAS_CONTR ) , 0 )
             INTO
               :RUFIB044.NMOTVO-DCLAS-CONTR
             FROM  DB2PRD.TMOTVO_DCLAS_CONTR
             WHERE CCONTR              = :RUFIB044.CCONTR
           END-EXEC.

           IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
              ( SQLWARN0               EQUAL 'W' )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'TMOTVO_DCLAS_CONTR' TO ERR-DBD-TAB
                MOVE 'SELECT  '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '2203'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2203-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2204-INSERIR-REGISTROS          SECTION.
      *---------------------------------------------------------------*

           ADD  1                     TO NMOTVO-DCLAS-CONTR OF RUFIB044.
BRQ001     MOVE NMOTVO-DCLAS-CONTR OF RUFIB044
BRQ001                                TO WRK-PIC9-5-CS.
BRQ001     MOVE WRK-PIC9-5-SS         TO WRK-INP-NMOTVO-T651
7COMMP                                   WRK-OUT-NMOTVO-T651.
7COMMP*                                 WWRK-INP-NMOTVO-T651.
BRQ001     MOVE SPACES                TO CREFT-RECOR OF RUFIB044.
BRQ001     MOVE -1                    TO WRK-CREFT-RECOR-NULL.

           EXEC SQL
             INSERT INTO DB2PRD.TMOTVO_DCLAS_CONTR
             ( CCONTR
             , NMOTVO_DCLAS_CONTR
             , CMOTVO_DCLAS_OPER
             , DDCLAS_OPER_CREDT
             , VPRINC_DCLAS_CONTR
             , CUSUAR_INCL
             , HINCL
             , CUSUAR_MANUT
             , HMANUT_REG
BRQ001       , CREFT_RECOR          )
             VALUES
             ( :RUFIB044.CCONTR
             , :RUFIB044.NMOTVO-DCLAS-CONTR
             , :RUFIB044.CMOTVO-DCLAS-OPER
             , :RUFIB044.DDCLAS-OPER-CREDT
             , :RUFIB044.VPRINC-DCLAS-CONTR
             , :RUFIB044.CUSUAR-INCL
             ,  CURRENT TIMESTAMP
             , :RUFIB044.CUSUAR-MANUT  :WRK-CUSUAR-MANUT-NULL
             , :RUFIB044.HMANUT-REG    :WRK-HMANUT-NULL
BRQ001       , :RUFIB044.CREFT-RECOR   :WRK-CREFT-RECOR-NULL   )
           END-EXEC.

           IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
              ( SQLWARN0               EQUAL 'W' )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'TMOTVO_DCLAS_CONTR' TO ERR-DBD-TAB
                MOVE 'INSERT  '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '2204'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2204-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

NAV506*---------------------------------------------------------------*
NAV506*2205-ENVIAR-COR0003             SECTION.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*
NAV506*   -MOVE 'RUCA6651'              TO WRK-PGM-CHAMADOR
NAV506*   -MOVE WRK-INP-AGENCIA-T651    TO WRK-AGENCIA8870
NAV506*   -MOVE WRK-INP-NROPER-T651     TO WRK-CEDULA
NAV506*   -IF WRK-INP-CGCF-T651      EQUAL ZEROES
NAV506*       -MOVE 'F'                 TO WRK-TIPO-PESSOA
NAV506*       -MOVE WRK-INP-CGCR-T651-X TO WRK-CPF-BAS
NAV506*       -MOVE WRK-INP-CGCD-T651-X TO WRK-CPF-DIG
NAV506*       -MOVE WRK-CPF-X           TO WRK-PICX-15
NAV506*       -MOVE WRK-PIC9-15         TO WRK-IDENTIF
NAV506*   -ELSE
NAV506*       -MOVE 'J'                 TO WRK-TIPO-PESSOA
NAV506*       -MOVE WRK-INP-CGCR-T651-X TO WRK-CGC-BAS
NAV506*       -MOVE WRK-INP-CGCF-T651-X TO WRK-CGC-FILI
NAV506*       -MOVE WRK-INP-CGCD-T651-X TO WRK-CGC-DIG
NAV506*       -MOVE WRK-CGC-X           TO WRK-PICX-15
NAV506*       -MOVE WRK-PIC9-15         TO WRK-IDENTIF
NAV506*   -END-IF.
NAV506*   -MOVE CREFT-RECOR OF RUFIV002 TO WRK-NUMREFORIG-X.
NAV506*   -MOVE 'RUEC8870'              TO WRK-MODULO.
NAV506*
NAV506*   -CALL    WRK-MODULO        USING  WRK-AREA-8870.
NAV506*
NAV506*   -IF  WRK-COD-RET           EQUAL  88
NAV506*       -MOVE SQLCA               TO WRK-SQLCA
NAV506*       -PERFORM 9999-ROTINA-ERRO
NAV506*   -END-IF.
NAV506*
NAV506*   -IF  WRK-COD-RET             NOT EQUAL ZEROS
NAV506*       -MOVE 'APL'               TO ERR-TIPO-ACESSO
NAV506*       -MOVE  RETURN-CODE        TO WRK-RETURN-CODE
NAV506*       -MOVE  10                 TO WRK-LOCAL-ERRO
NAV506*       -MOVE  WRK-MENSAGEM8870   TO ERR-TEXTO
NAV506*       -PERFORM 9999-ROTINA-ERRO
NAV506*   -END-IF.
NAV506*
NAV506*   -MOVE '3'                    TO WRK-OUT-FASE-T651
NAV506*   -MOVE 'CONSULTA COR0003 ENVIADA'
NAV506*                                TO WRK-OUT-MENSAG-T651
NAV506*   -MOVE WRK-INP-NROPER-T651-X  TO WRK-OUT-NROPER-T651-X
NAV506*   -MOVE WRK-INP-DIGOPER-T651   TO WRK-OUT-DIGOPER-T651
NAV506*   -MOVE WRK-INP-AGENCIA-T651-X TO WRK-OUT-AGENCIA-T651-X
NAV506*   -MOVE WRK-INP-NOMAG-T651     TO WRK-OUT-NOMAG-T651
NAV506*   -MOVE WRK-INP-AAFINAN-T651-X TO WRK-OUT-AAFINAN-T651-X
NAV506*   -MOVE WRK-INP-NOME-T651      TO WRK-OUT-NOME-T651
NAV506*   -MOVE WRK-INP-CC-T651-X      TO WRK-OUT-CC-T651-X
NAV506*   -MOVE WRK-INP-CCDIG-T651-X   TO WRK-OUT-CCDIG-T651-X
NAV506*   -MOVE WRK-INP-CGCR-T651-X    TO WRK-OUT-CGCR-T651-X
NAV506*   -MOVE WRK-INP-CGCF-T651-X    TO WRK-OUT-CGCF-T651-X
NAV506*   -MOVE WRK-INP-CGCD-T651-X    TO WRK-OUT-CGCD-T651-X
NAV506*   -MOVE WRK-INP-CREDITO-T651   TO WRK-OUT-CREDITO-T651-X
NAV506*   -MOVE WRK-INP-DEVEDOR-T651   TO WRK-OUT-DEVEDOR-T651-X
NAV506*   -MOVE WRK-INP-VLRDESC-T651   TO WRK-OUT-VLRDESC-T651-X
NAV506*   -MOVE WRK-INP-DTDESC-T651    TO WRK-OUT-DTDESC-T651
NAV506*   -MOVE WRK-INP-MOTDES-T651-X  TO WRK-OUT-MOTDES-T651-X
NAV506*   -MOVE WRK-INP-MOTDES1-T651   TO WRK-OUT-MOTDES1-T651.
NAV506*
NAV506*   -MOVE WRK-CONTROLE-BACEN     TO WRK-INP-CTRLIF-T651
NAV506*                                   WRK-OUT-CTRLIF-T651.
NAV506*
NAV506*   -MOVE WRK-225                TO WRK-OUT-OPER-ATTR-T651
NAV506*                                   WRK-OUT-VLRDESC-ATTR-T651
NAV506*                                   WRK-OUT-DTDESC-ATTR-T651
NAV506*                                   WRK-OUT-MOTDES-ATTR-T651.
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2205-99-FIM.                    EXIT.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2206-COUNT-RUECB09C             SECTION.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*   -INITIALIZE RUECB09C.
NAV506*
NAV506*   -MOVE WRK-INP-CTRLIF-T651    TO CCTRL-RQUIS-FINCR OF RUECB09C.
NAV506*
NAV506*   -EXEC SQL
NAV506*        -SELECT COUNT(*)
NAV506*          -INTO :WRK-TOTAL-COR0003R1
NAV506*          -FROM DB2PRD.TMNTRC_MSGEM_RURAL
NAV506*         -WHERE CCTRL_RQUIS_FINCR = :RUECB09C.CCTRL-RQUIS-FINCR
NAV506*           -AND CMSGEM_RECOR      = 'COR0003R1'
NAV506*   -END-EXEC.
NAV506*
NAV506*   -IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
NAV506*       ( SQLWARN0               EQUAL 'W' )
NAV506*        -MOVE 'DB2'                TO ERR-TIPO-ACESSO
NAV506*        -MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
NAV506*        -MOVE 'SELECT  '           TO ERR-FUN-COMANDO
NAV506*        -MOVE SQLCODE              TO ERR-SQL-CODE
NAV506*        -MOVE '2206'               TO ERR-LOCAL
NAV506*        -MOVE SPACES               TO ERR-SEGM
NAV506*        -PERFORM 9999-ROTINA-ERRO
NAV506*   -END-IF.
NAV506*
NAV506*   -IF SQLCODE                  EQUAL +100
NAV506*       -MOVE ZEROS                 TO WRK-TOTAL-COR0003R1
NAV506*   -END-IF.
NAV506*
NAV506*   -MOVE WRK-225                TO WRK-OUT-OPER-ATTR-T651
NAV506*                                   WRK-OUT-VLRDESC-ATTR-T651
NAV506*                                   WRK-OUT-DTDESC-ATTR-T651
NAV506*                                   WRK-OUT-MOTDES-ATTR-T651.
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2206-99-FIM.                    EXIT.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2207-OPEN-COR0003R1             SECTION.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*   -INITIALIZE RUECB09C.
NAV506*   -MOVE 'N'                    TO WRK-ULT-CHAMADA.
NAV506*
NAV506*   -MOVE WRK-INP-CTRLIF-T651    TO CCTRL-RQUIS-FINCR OF RUECB09C.
NAV506*
NAV506*   -EXEC SQL
NAV506*        -OPEN CSR01-RUECB09C
NAV506*   -END-EXEC.
NAV506*
NAV506*   -IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
NAV506*       ( SQLWARN0               EQUAL 'W' )
NAV506*        -MOVE 'DB2'                TO ERR-TIPO-ACESSO
NAV506*        -MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
NAV506*        -MOVE 'OPEN    '           TO ERR-FUN-COMANDO
NAV506*        -MOVE SQLCODE              TO ERR-SQL-CODE
NAV506*        -MOVE '2207'               TO ERR-LOCAL
NAV506*        -MOVE SPACES               TO ERR-SEGM
NAV506*        -PERFORM 9999-ROTINA-ERRO
NAV506*   -END-IF.
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2207-99-FIM.                    EXIT.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2208-FETCH-COR0003R1            SECTION.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*   -EXEC SQL
NAV506*        -FETCH CSR01-RUECB09C
NAV506*         -INTO :RUECB09C.WCONTD-MSGEM-RURAL
NAV506*   -END-EXEC.
NAV506*
NAV506*   -IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
NAV506*       ( SQLWARN0               EQUAL 'W' )
NAV506*        -MOVE 'DB2'                TO ERR-TIPO-ACESSO
NAV506*        -MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
NAV506*        -MOVE 'FETCH   '           TO ERR-FUN-COMANDO
NAV506*        -MOVE SQLCODE              TO ERR-SQL-CODE
NAV506*        -MOVE '2208'               TO ERR-LOCAL
NAV506*        -MOVE SPACES               TO ERR-SEGM
NAV506*        -PERFORM 9999-ROTINA-ERRO
NAV506*   -END-IF.
NAV506*
NAV506*   -IF SQLCODE                  EQUAL +100
NAV506*       -GO TO 2208-99-FIM
NAV506*   -END-IF.
NAV506*
NAV506*   -ADD 1                       TO ACU-FETCH-COR0003.
NAV506*
NAV506*   -IF ACU-FETCH-COR0003        EQUAL WRK-TOTAL-COR0003R1
NAV506*      -MOVE 'S'                 TO WRK-ULT-CHAMADA
NAV506*   -ELSE
NAV506*      -MOVE 'N'                 TO WRK-ULT-CHAMADA
NAV506*   -END-IF.
NAV506*
NAV506*   -PERFORM 2210-ENVIO-COR0001.
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2208-99-FIM.                    EXIT.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2209-CLOSE-COR0003R1            SECTION.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*   -EXEC SQL
NAV506*        -CLOSE CSR01-RUECB09C
NAV506*   -END-EXEC.
NAV506*
NAV506*   -IF ( SQLCODE                NOT EQUAL ZEROS )   OR
NAV506*       ( SQLWARN0               EQUAL 'W' )
NAV506*        -MOVE 'DB2'                TO ERR-TIPO-ACESSO
NAV506*        -MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
NAV506*        -MOVE 'CLOSE   '           TO ERR-FUN-COMANDO
NAV506*        -MOVE SQLCODE              TO ERR-SQL-CODE
NAV506*        -MOVE '2209'               TO ERR-LOCAL
NAV506*        -MOVE SPACES               TO ERR-SEGM
NAV506*        -PERFORM 9999-ROTINA-ERRO
NAV506*   -END-IF.
NAV506*
NAV506*   -MOVE WRK-225                TO WRK-OUT-OPER-ATTR-T651
NAV506*                                   WRK-OUT-VLRDESC-ATTR-T651
NAV506*                                   WRK-OUT-DTDESC-ATTR-T651
NAV506*                                   WRK-OUT-MOTDES-ATTR-T651.
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2209-99-FIM.                    EXIT.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2210-ENVIO-COR0001              SECTION.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*   -MOVE WCONTD-MSGEM-RURAL-TEXT OF RUECB09C
NAV506*                                 TO WRK-MSGEM-COR0003-TEXT.
NAV506*   -MOVE 'RUCA6651'              TO WRK-PGM-CHAMADOR-8871.
NAV506*   -MOVE WRK-INP-AGENCIA-T651    TO WRK-AGENCIA-8871.
NAV506*   -MOVE WRK-INP-NROPER-T651     TO WRK-CEDULA-8871.
NAV506*   -MOVE WRK-PIC9-3-SS           TO WRK-NUM-SEQ-DESCLA.
NAV506*   -MOVE 'I'                     TO WRK-FUNCAO-8871.
NAV506*   -MOVE 14                     TO WRK-POOL0496-TAM-INT.
NAV506*   -MOVE 02                     TO WRK-POOL0496-TAM-DEC.
NAV506*   -MOVE WRK-INP-CREDITO-T651   TO WRK-POOL0496-ENTRADA.
NAV506*   -MOVE ZEROS                  TO RETURN-CODE
NAV506*                                   WRK-POOL0496-RETURN-CODE.
NAV506*   -MOVE SPACES                 TO WRK-POOL0496-SAIDA.
NAV506*
NAV506*   -CALL 'POOL0496'             USING WRK-POOL0496-ENTRADA
NAV506*                                      WRK-POOL0496-SAIDA
NAV506*                                      WRK-POOL0496-TAMANHO.
NAV506*
NAV506*   -MOVE RETURN-CODE            TO WRK-POOL0496-RETURN-CODE.
NAV506*
NAV506*   -IF  WRK-POOL0496-RETURN-CODE EQUAL ZEROS OR 20
NAV506*       -MOVE WRK-POOL0496-SAIDA TO WRK-CH21
NAV506*       -MOVE WRK-ZD14V2         TO WRK-VALOR-TOT-FINANC
NAV506*                                   WRK-VLRTOTOP
NAV506*   -ELSE
NAV506*       -MOVE ZEROS              TO WRK-VALOR-TOT-FINANC
NAV506*                                   WRK-VLRTOTOP
NAV506*   -END-IF.
NAV506*
NAV506*   -MOVE 14                     TO WRK-POOL0496-TAM-INT.
NAV506*   -MOVE 02                     TO WRK-POOL0496-TAM-DEC.
NAV506*   -MOVE WRK-INP-VLRDESC-T651   TO WRK-POOL0496-ENTRADA.
NAV506*   -MOVE ZEROS                  TO RETURN-CODE
NAV506*                                   WRK-POOL0496-RETURN-CODE.
NAV506*   -MOVE SPACES                 TO WRK-POOL0496-SAIDA.
NAV506*
NAV506*   -CALL 'POOL0496'             USING WRK-POOL0496-ENTRADA
NAV506*                                      WRK-POOL0496-SAIDA
NAV506*                                      WRK-POOL0496-TAMANHO.
NAV506*
NAV506*   -MOVE RETURN-CODE            TO WRK-POOL0496-RETURN-CODE.
NAV506*
NAV506*   -IF  WRK-POOL0496-RETURN-CODE EQUAL ZEROS OR 20
NAV506*       -MOVE WRK-POOL0496-SAIDA TO WRK-CH21
NAV506*       -MOVE WRK-ZD14V2         TO WRK-VALOR-DESCL
NAV506*   -ELSE
NAV506*       -MOVE ZEROS              TO WRK-VALOR-DESCL
NAV506*   -END-IF.
NAV506*   -MOVE WRK-VALOR-DESCL        TO WRK-VALOR-DESCL-ATU
NAV506*                                   WRK-VLRPRINCIPALPARCL.
NAV506*   -COMPUTE WRK-RESTO =
NAV506*           (WRK-VALOR-TOT-FINANC - WRK-VALOR-DESCL-ATU).
NAV506*
NAV506*   -IF WRK-RESTO              EQUAL ZEROS
NAV506*      -MOVE 'T'                  TO WRK-TP-DESCLASS
NAV506*   -ELSE
NAV506*      -MOVE 'P'                  TO WRK-TP-DESCLASS
NAV506*   -END-IF.
NAV506*
NAV506*   -MOVE WRK-INP-CTRLIF-T651     TO WRK-NUM-CTRL-IF.
NAV506*   -MOVE 'RUEC8871'              TO WRK-MODULO.
NAV506*
NAV506*   -CALL WRK-MODULO           USING WRK-AREA-8871.
NAV506*
NAV506*   -IF RETURN-CODE EQUAL ZEROES
NAV506*      -ADD 1 TO ACU-ENVIO-COR0001
NAV506*      -MOVE WRK-CONTROLE-BACEN-COR0001
NAV506*                                 TO WRK-INP-CTRLIF-T651
NAV506*                                    WRK-OUT-CTRLIF-T651
NAV506*   -END-IF.
NAV506*   -IF RETURN-CODE NOT EQUAL ZEROES AND +100
NAV506*       -MOVE 'S'                TO WRK-INCONSISTENCIA
NAV506*       -MOVE 'ERRO DO MODULO DE ENVIO COR0001'
NAV506*                                TO WRK-OUT-MENSAG-T651
NAV506*   -END-IF.
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2210-99-FIM.                    EXIT.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2211-SELECT-COR0001R1           SECTION.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*   -INITIALIZE RUECB09C.
NAV506*
NAV506*   -MOVE WRK-INP-CTRLIF-T651    TO CCTRL-RQUIS-FINCR OF RUECB09C.
NAV506*
NAV506*   -EXEC SQL
NAV506*        -SELECT CMSGEM_RECOR,
NAV506*                WCONTD_MSGEM_RURAL
NAV506*          -INTO :RUECB09C.CMSGEM-RECOR
NAV506*                :WRK-CMSGEM-RECOR-NULL,
NAV506*                :RUECB09C.WCONTD-MSGEM-RURAL
NAV506*          -FROM DB2PRD.TMNTRC_MSGEM_RURAL
NAV506*         -WHERE CCTRL_RQUIS_FINCR = :RUECB09C.CCTRL-RQUIS-FINCR
NAV506*           -AND NMNTRC_MSGEM_RURAL >  0
NAV506*           -AND CMSGEM_RECOR IN ('COR0001R1' , 'COR0001E')
NAV506*           -ORDER BY NMNTRC_MSGEM_RURAL DESC
NAV506*           -FETCH FIRST 1 ROW ONLY
NAV506*   -END-EXEC.
NAV506*
NAV506*   -IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
NAV506*       ( SQLWARN0               EQUAL 'W' )
NAV506*        -MOVE 'DB2'                TO ERR-TIPO-ACESSO
NAV506*        -MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
NAV506*        -MOVE 'SELECT  '           TO ERR-FUN-COMANDO
NAV506*        -MOVE SQLCODE              TO ERR-SQL-CODE
NAV506*        -MOVE '2211'               TO ERR-LOCAL
NAV506*        -MOVE SPACES               TO ERR-SEGM
NAV506*        -PERFORM 9999-ROTINA-ERRO
NAV506*   -END-IF.
NAV506*
NAV506*   -IF WRK-CMSGEM-RECOR-NULL    LESS ZEROS
NAV506*      -MOVE SPACES              TO CMSGEM-RECOR OF RUECB09C
NAV506*   -END-IF.
NAV506*
NAV506*   -IF SQLCODE                  EQUAL +100
NAV506*      -MOVE ZEROS               TO CREFT-RECOR OF    RUFIB044
NAV506*                                   WRK-CREFT-RECOR-NULL
NAV506*      -MOVE 'AGUARDANDO RETORNO BACEN - COR0001'
NAV506*                                TO WRK-OUT-MENSAG-T651
NAV506*      -MOVE WRK-225             TO WRK-OUT-OPER-ATTR-T651
NAV506*                                   WRK-OUT-VLRDESC-ATTR-T651
NAV506*                                   WRK-OUT-DTDESC-ATTR-T651
NAV506*                                   WRK-OUT-MOTDES-ATTR-T651
NAV506*   -ELSE
NAV506*     -IF CMSGEM-RECOR OF RUECB09C EQUAL 'COR0001E'
NAV506*        -MOVE 'RETORNO BACEN COM ERRO - COR0001E'
NAV506*                                TO WRK-OUT-MENSAG-T651
NAV506*        -MOVE WRK-225           TO WRK-OUT-OPER-ATTR-T651
NAV506*                                   WRK-OUT-VLRDESC-ATTR-T651
NAV506*                                   WRK-OUT-DTDESC-ATTR-T651
NAV506*                                   WRK-OUT-MOTDES-ATTR-T651
NAV506*     -ELSE
NAV506*        -MOVE WCONTD-MSGEM-RURAL-TEXT
NAV506*                                TO WRK-WCONTD-MSGEM-RURAL-AUX
NAV506*        -PERFORM 2212-UPDATE-RUFIB044
NAV506*     -END-IF
NAV506*   -END-IF.
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2211-99-FIM.                    EXIT.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2212-UPDATE-RUFIB044            SECTION.
NAV506*---------------------------------------------------------------*
NAV506*
NAV506*   -INITIALIZE RUFIB044.
NAV506*
NAV506*   -MOVE WRK-INP-NROPER-T651  TO CCONTR      OF    RUFIB044.
NAV506*   -MOVE WRK-NUMREFBCCOR-COR1 TO CREFT-RECOR OF    RUFIB044.
NAV506*   -MOVE ZEROS                TO WRK-CREFT-RECOR-NULL.
NAV506*   -MOVE WRK-INP-NMOTVO-T651  TO NMOTVO-DCLAS-CONTR OF RUFIB044
NAV506*   -MOVE WRK-COD-USER-X       TO CUSUAR-MANUT       OF RUFIB044.
NAV506*
NAV506*   -EXEC SQL
NAV506*       -UPDATE DB2PRD.TMOTVO_DCLAS_CONTR
NAV506*          -SET CREFT_RECOR        = :RUFIB044.CREFT-RECOR
NAV506*                                    :WRK-CREFT-RECOR-NULL,
NAV506*               CUSUAR_MANUT       = :RUFIB044.CUSUAR-MANUT,
NAV506*               HMANUT_REG         = CURRENT_TIMESTAMP
NAV506*        -WHERE CCONTR             = :RUFIB044.CCONTR
NAV506*          -AND NMOTVO_DCLAS_CONTR = :RUFIB044.NMOTVO-DCLAS-CONTR
NAV506*   -END-EXEC.
NAV506*
NAV506*   -IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
NAV506*       ( SQLWARN0               EQUAL 'W' )
NAV506*        -MOVE 'DB2'                TO ERR-TIPO-ACESSO
NAV506*        -MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
NAV506*        -MOVE 'SELECT  '           TO ERR-FUN-COMANDO
NAV506*        -MOVE SQLCODE              TO ERR-SQL-CODE
NAV506*        -MOVE '2212'               TO ERR-LOCAL
NAV506*        -MOVE SPACES               TO ERR-SEGM
NAV506*        -PERFORM 9999-ROTINA-ERRO
NAV506*   -END-IF.
NAV506*
NAV506*    -PERFORM 2213-INCLUSAO-EFETUADA.
NAV506*
NAV506*---------------------------------------------------------------*
NAV506*2212-99-FIM.                    EXIT.
NAV506*---------------------------------------------------------------*
BRQ001
BRQ001*---------------------------------------------------------------*
BRQ001 2213-INCLUSAO-EFETUADA          SECTION.
BRQ001*---------------------------------------------------------------*
BRQ001
BRQ001     MOVE '1'                    TO WRK-OUT-FASE-T651
BRQ001     MOVE 'INCLUSAO EFETUADA COM SUCESSO'
BRQ001                                 TO WRK-OUT-MENSAG-T651
BRQ001     MOVE WRK-49361              TO WRK-OUT-OPER-ATTR-T651
BRQ001                                    WRK-OUT-VLRDESC-ATTR-T651
BRQ001                                    WRK-OUT-DTDESC-ATTR-T651
BRQ001                                    WRK-OUT-MOTDES-ATTR-T651.
BRQ001
BRQ001     MOVE SPACES                 TO WRK-OUT-NROPER-T651-X
BRQ001                                    WRK-OUT-DIGOPER-T651
BRQ001                                    WRK-OUT-AGENCIA-T651-X
BRQ001                                    WRK-OUT-NOMAG-T651
BRQ001                                    WRK-OUT-AAFINAN-T651-X
BRQ001                                    WRK-OUT-NOME-T651
BRQ001                                    WRK-OUT-CC-T651-X
BRQ001                                    WRK-OUT-CCDIG-T651-X
BRQ001                                    WRK-OUT-CGCR-T651-X
BRQ001                                    WRK-OUT-CGCF-T651-X
BRQ001                                    WRK-OUT-CGCD-T651-X
BRQ001                                    WRK-OUT-CREDITO-T651-X
BRQ001                                    WRK-OUT-DEVEDOR-T651-X
BRQ001                                    WRK-OUT-VLRDESC-T651-X
BRQ001                                    WRK-OUT-DTDESC-T651
BRQ001                                    WRK-OUT-MOTDES-T651-X
BRQ001                                    WRK-OUT-MOTDES1-T651.
BRQ001
BRQ001*---------------------------------------------------------------*
BRQ001 2213-99-FIM.                    EXIT.
BRQ001*---------------------------------------------------------------*
BRQ001
BRQ001*---------------------------------------------------------------*
BRQ001 2214-VERIFICA-DESCLA-TOT        SECTION.
BRQ001*---------------------------------------------------------------*
BRQ001
BRQ001     MOVE 14                     TO WRK-POOL0496-TAM-INT.
BRQ001     MOVE 02                     TO WRK-POOL0496-TAM-DEC.
BRQ001     MOVE WRK-INP-CREDITO-T651   TO WRK-POOL0496-ENTRADA.
BRQ001     MOVE ZEROS                  TO RETURN-CODE
BRQ001                                    WRK-POOL0496-RETURN-CODE.
BRQ001     MOVE SPACES                 TO WRK-POOL0496-SAIDA.
BRQ001
BRQ001     CALL 'POOL0496'             USING WRK-POOL0496-ENTRADA
BRQ001                                       WRK-POOL0496-SAIDA
BRQ001                                       WRK-POOL0496-TAMANHO.
BRQ001
BRQ001     MOVE RETURN-CODE            TO WRK-POOL0496-RETURN-CODE.
BRQ001
BRQ001     IF  WRK-POOL0496-RETURN-CODE EQUAL ZEROS OR 20
BRQ001         MOVE WRK-POOL0496-SAIDA TO WRK-CH21
BRQ001         MOVE WRK-ZD14V2         TO WRK-VALOR-TOT-FINANC
BRQ001     ELSE
BRQ001         MOVE ZEROS              TO WRK-VALOR-TOT-FINANC
BRQ001     END-IF.
BRQ001
BRQ001     MOVE 14                     TO WRK-POOL0496-TAM-INT.
BRQ001     MOVE 02                     TO WRK-POOL0496-TAM-DEC.
BRQ001     MOVE WRK-INP-VLRDESC-T651   TO WRK-POOL0496-ENTRADA.
BRQ001     MOVE ZEROS                  TO RETURN-CODE
BRQ001                                    WRK-POOL0496-RETURN-CODE.
BRQ001     MOVE SPACES                 TO WRK-POOL0496-SAIDA.
BRQ001
BRQ001     CALL 'POOL0496'             USING WRK-POOL0496-ENTRADA
BRQ001                                       WRK-POOL0496-SAIDA
BRQ001                                       WRK-POOL0496-TAMANHO.
BRQ001
BRQ001     MOVE RETURN-CODE            TO WRK-POOL0496-RETURN-CODE.
BRQ001
BRQ001     IF  WRK-POOL0496-RETURN-CODE EQUAL ZEROS OR 20
BRQ001         MOVE WRK-POOL0496-SAIDA TO WRK-CH21
BRQ001         MOVE WRK-ZD14V2         TO WRK-VALOR-DESCL
BRQ001     ELSE
BRQ001         MOVE ZEROS              TO WRK-VALOR-DESCL
BRQ001     END-IF.
BRQ001
BRQ001     MOVE WRK-VALOR-DESCL        TO WRK-VALOR-DESCL-ATU.
BRQ001
BRQ001     COMPUTE WRK-RESTO =
BRQ001            (WRK-VALOR-TOT-FINANC - WRK-VALOR-DESCL-ATU).
BRQ001
BRQ001     IF WRK-RESTO              EQUAL ZEROS
BRQ001        MOVE 'T'                  TO WRK-TP-DESCLASS
BRQ001     ELSE
BRQ001        MOVE 'P'                  TO WRK-TP-DESCLASS
BRQ001     END-IF.
BRQ001
BRQ001*---------------------------------------------------------------*
BRQ001 2214-99-FIM.                    EXIT.
BRQ001*---------------------------------------------------------------*
BRQ001
WP1122*----------------------------------------------------------------*
-      2220-ENVIAR-COR0005             SECTION.
-     *----------------------------------------------------------------*
-
-          INITIALIZE RUEC65-AREA.
-
-          MOVE 'RUCA6651'             TO RUEC65-PGM-CHAMADOR
           MOVE WRK-INP-NROPER-T651    TO RUEC65-CCONTR
-          MOVE ZEROS                  TO RUEC65-NENVIO-RECOR
-          MOVE 'F'                    TO RUEC65-FUNCAO
NAV506     MOVE WRK-TP-DESCLASS        TO RUEC65-TP-DESCLASS
-
-          MOVE 'RUEC8831'             TO WRK-MODULO.
-
-          CALL WRK-MODULO             USING RUEC65-AREA
-
-
-          EVALUATE RUEC65-COD-RET
-             WHEN ZEROES
-                  MOVE 'COR0005 ENVIADA BACEN'
-                                           TO WRK-OUT-MENSAG-T651
-             WHEN 99
-                  MOVE RUEC65-SQLCA        TO SQLCA
-                  MOVE RUEC65-ERRO-AREA    TO ERRO-AREA
-                  MOVE 'APL'               TO ERR-TIPO-ACESSO
-                  MOVE RUEC65-COD-RET      TO WRK-RETURN-CODE
-                  MOVE 11                  TO WRK-LOCAL-ERRO
-                  MOVE RUEC65-MENSAGEM     TO ERR-TEXTO
-                  PERFORM 9999-ROTINA-ERRO
-             WHEN OTHER
-                  MOVE RUEC65-MENSAGEM    TO WRK-OUT-MENSAG-T651
-          END-EVALUATE.
-
-     *----------------------------------------------------------------*
-      2220-99-FIM.    EXIT.
WP2211*----------------------------------------------------------------*
      *---------------------------------------------------------------*
       2221-ALTERAR-RUFIV002          SECTION.
      *---------------------------------------------------------------*

AKII       MOVE 140                 TO CORIGE-REC OF RUFIV002.
           MOVE WRK-INP-NROPER-T651 TO CCONTR     OF RUFIV002.

           EXEC SQL
              UPDATE DB2PRD.V01CONTRATO_RURAL
              SET    CORIGE_REC     = :RUFIV002.CORIGE-REC
              WHERE  CCONTR         = :RUFIV002.CCONTR
           END-EXEC


           IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
              ( SQLWARN0               EQUAL 'W' )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'V01CONTRATO_RURAL'  TO ERR-DBD-TAB
                MOVE 'UPDATE  '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '2221'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2221-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2222-ALTERAR-RUCAB003          SECTION.
      *---------------------------------------------------------------*

           PERFORM 2152-OBTER-DADOS-RUCAB003.

           IF B003-ORIGREC1 OF RUCAB003 EQUAL ZEROS
              MOVE 140 TO B003-ORIGREC1 OF RUCAB003
              PERFORM 2223-UPDT-RUCAB003
              GO TO 2222-99-FIM
           END-IF.

           IF B003-ORIGREC2 OF RUCAB003 EQUAL ZEROS
              MOVE 140 TO B003-ORIGREC2 OF RUCAB003
              PERFORM 2223-UPDT-RUCAB003
              GO TO 2222-99-FIM
           END-IF.

           IF B003-ORIGREC3 OF RUCAB003 EQUAL ZEROS
              MOVE 140 TO B003-ORIGREC3 OF RUCAB003
              PERFORM 2223-UPDT-RUCAB003
              GO TO 2222-99-FIM
           END-IF.

           MOVE 140 TO B003-ORIGREC4 OF RUCAB003.
           PERFORM 2223-UPDT-RUCAB003.

      *---------------------------------------------------------------*
       2222-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2223-UPDT-RUCAB003          SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-INP-NROPER-T651    TO B003-CH-CONTR OF  RUCAB003.

           EXEC SQL
              UPDATE DB2PRD.RUCA_CAD_ROOT
              SET    B003_ORIGREC1 = :RUCAB003.B003-ORIGREC1,
                     B003_ORIGREC2 = :RUCAB003.B003-ORIGREC2,
                     B003_ORIGREC3 = :RUCAB003.B003-ORIGREC3,
                     B003_ORIGREC4 = :RUCAB003.B003-ORIGREC4
              WHERE  B003_CH_CONTR = :RUCAB003.B003-CH-CONTR
           END-EXEC

           IF ( SQLCODE                NOT EQUAL ZEROS AND +100)   OR
              ( SQLWARN0               EQUAL 'W' )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'RUCA_CAD_ROOT'      TO ERR-DBD-TAB
                MOVE 'UPDATE  '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '2222'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2223-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2300-TRATAR-PF3                 SECTION.
      *---------------------------------------------------------------*

           MOVE 'RUCA6896'             TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.

      *---------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
201609*----------------------------------------------------------------*
201609 2340-PASSAR-CONTROLE-RURC2770   SECTION.
201609*----------------------------------------------------------------*
201609
201609     IF  WRK-INP-LIN-T651        NOT EQUAL 18
201609     OR (WRK-INP-COL-T651        NOT EQUAL 54 AND 55 AND 56 )
201609         MOVE 'POSICIONE O CURSOR NO CAMPO MOTIVO PARA ACIONAR A L
201609-              'ISTA'            TO WRK-OUT-MENSAG-T651
201609         GO TO 2340-99-FIM
201609     END-IF.
201609
201609     INITIALIZE WRK-2770-AREA.
201609     MOVE LENGTH OF WRK-2770-AREA TO WRK-2770-LL
201609     MOVE ZEROS                  TO  WRK-2770-ZZ
201609     MOVE 'RUCA6651'             TO  WRK-2770-TRANSACAO
201609     MOVE '.'                    TO  WRK-2770-PFK
201609     MOVE WRK-INP-SENHAS-T651    TO  WRK-2770-SENHAS
201609     MOVE ZEROS                  TO  WRK-2770-OPCAO
201609                                     WRK-2770-MOTIVO-SELCD
201609     MOVE SPACES                 TO  WRK-2770-MENSAGEM
201609     MOVE 'RUCA6651'             TO  WRK-2770-ANTE-TELA
201609     MOVE WRK-INPUT-T651(1:242)  TO  WRK-2770-NAVEGACAO.
201609
201609     MOVE WRK-2770-AREA          TO  WRK-MENSAGEM.
201609     MOVE WRK-CHNG               TO  WRK-FUNCAO.
201609     MOVE 'RURC2770'             TO  WRK-TELA.
201609
201609*----------------------------------------------------------------*
201609 2340-99-FIM.                    EXIT.
201609*----------------------------------------------------------------*
201609
      *---------------------------------------------------------------*
       4000-POOL0660-RUCAT651          SECTION.
      *---------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-T651
                                       TO WRK-660-LL-MENSAGEM-T651
                                          WRK-OUT-LL-T651
           MOVE LENGTH OF WRK-660-RUCAT651
                                       TO WRK-660-LL-AREA-T651

           CALL  'POOL0660'    USING   WRK-OUTPUT-T651
                                       WRK-660-RUCAT651.

           IF RETURN-CODE              NOT EQUAL ZEROS
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO WRK-RETURN-CODE
              MOVE  11                 TO WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-POOL0660  TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO WRK-FUNCAO.
           MOVE  WRK-OUTPUT-T651       TO WRK-MENSAGEM.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *---------------------------------------------------------------*

           MOVE 'SENHAS02'             TO ERR-PGM.
           MOVE 'RUCA6651'             TO ERR-MODULO
           MOVE  WRK-COD-USER-X        TO ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO ERR-COD-DEPTO.

           CALL 'POOL7100'             USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             SQLCA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
