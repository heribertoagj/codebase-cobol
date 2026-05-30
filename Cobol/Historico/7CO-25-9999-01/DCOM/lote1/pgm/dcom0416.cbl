      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM0416.
       AUTHOR.         ADRIANO RICARDO BELMONT.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0416                                     *
      *     PROGRAMADOR.: ADRIANO RICARDO BELMONT                      *
      *     ANALISTA....: DANILO                                       *
      *     DATA........: 05/02/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LIBERAR CONTRATO - DETALHE                   *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMDPTM  -   LIBERAR OPERACAO - DEMONSTRATIVO             *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *----------------------------------------------------------------*
      *     MODULOS:                                                   *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7600 - FORNECE DATA E HORA CORRENTE DO SISTEMA         *
      *                                                                *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0413           CHAMA: DCOM0412           *
      *                   DCOM0414                                     *
      *                   DCOM0415                                     *
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
       DATA                             DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *           *** DCOM0416 - INICIO DA AREA DE WORKING ***         *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *                ***     AREA DE ATRIBUTOS ***                   *
      *----------------------------------------------------------------*

      ****  NUMERICO - NORMAL    -    PROTEGIDO  - NAO POS. CURSOR  ---*
       01  WRK-241                     PIC S9(008) COMP  VALUE +241.
      ****  NUMERICO - NORMAL    -    PROTEGIDO  -     POS. CURSOR  ---*
       01  WRK-49361                   PIC S9(008) COMP  VALUE +49361.
      ****  NUMERICO - BRILHANTE - DESPROTEGIDO  -     POS. CURSOR  ---*
       01  WRK-49369                   PIC S9(008) COMP  VALUE +49369.
      **** ALFA - BRILHANTE - NORMAL - DESPROTEGIDO  - POS CURSOR   ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.

      *----------------------------------------------------------------*
      *               *** VARIAVEIS AUXILIARES ***                     *
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-SQLCA-416           PIC  X(136)       VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)       VALUE SPACES.
           05  WRK-NUMERO              PIC  X(008)       VALUE ZEROS.
           05  WRK-IN                  PIC  9(002)       VALUE ZEROS.
           05  WRK-IN2                 PIC  9(002)       VALUE ZEROS.
           05  WRK-RETORNO             PIC  9(003)       VALUE ZEROS.
           05  WRK-RESTART             PIC  9(003)       VALUE ZEROS.
           05  WRK-INCONSIS            PIC  X(001)       VALUE SPACES.
           05  WRK-ERRO-ACESSO         PIC  X(001)       VALUE SPACES.
           05  WRK-SQLCA               PIC  X(136)       VALUE SPACES.
           05  WRK-EFETUADO            PIC  X(001)       VALUE 'N'.
           05  WRK-PERFIL              PIC  X(001)       VALUE 'N'.

       01  WRK-MENS-4128.
           05  WRK-RET-4128            PIC  X(004)       VALUE SPACES.
           05  WRK-TRACO-4128          PIC  X(001)       VALUE '-'.
           05  WRK-MENS-4128           PIC  X(074)       VALUE SPACES.

       01  WRK-MENS-5142.
           05  WRK-RET-5142            PIC  X(004)       VALUE SPACES.
           05  WRK-TRACO-5142          PIC  X(001)       VALUE '-'.
           05  WRK-MENS-5142           PIC  X(074)       VALUE SPACES.

           05  WRK-SENHAS.
               10  WRK-COD-FUNCIONARIO PIC  9(009)       VALUE ZEROS.
               10  WRK-COD-TERMINAL    PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(020)       VALUE SPACES.

           05  WRK-MASK-LIMITE         PIC  ZZZ.ZZZ.ZZ9,99.
           05  WRK-LIMITE  REDEFINES   WRK-MASK-LIMITE
                                       PIC  X(014).

       01  WRK-CNPJ-COMPL.
FS2511*    03  WRK-CNPJ1               PIC  9(03)      VALUE ZEROS.
FS2511     03  WRK-CNPJ1               PIC  X(03)      VALUE SPACES.
           03  WRK-PONTO1              PIC  X(01)      VALUE '.'.
FS2511*    03  WRK-CNPJ2               PIC  9(03)      VALUE ZEROS.
FS2511     03  WRK-CNPJ2               PIC  X(03)      VALUE SPACES.
           03  WRK-PONTO2              PIC  X(01)      VALUE '.'.
FS2511*    03  WRK-CNPJ3               PIC  9(03)      VALUE ZEROS.
FS2511     03  WRK-CNPJ3               PIC  X(03)      VALUE SPACES.
           03  WRK-BARRA-1             PIC  X(01)      VALUE '/'.
FS2511*    03  WRK-FILIAL              PIC  9(04)      VALUE ZEROS.
FS2511     03  WRK-FILIAL              PIC  X(04)      VALUE SPACES.
           03  WRK-TRACO-1             PIC  X(01)      VALUE '-'.
           03  WRK-CONTROLE            PIC  9(02)      VALUE ZEROS.

       01  WRK-AGENC-COMPL.
           03  WRK-AGENCIA             PIC  9(05)      VALUE ZEROS.
           03  WRK-TRACO-AG            PIC  X(01)      VALUE '-'.
           03  WRK-DESC-AG             PIC  X(20)      VALUE SPACES.

       01  AREA-BRAD0431.
           03  WRK-CONTA               PIC  9(13) COMP-3.
           03  WRK-CONTA-R             PIC  9(13)     VALUE ZEROS.
           03  WRK-DIGITO              PIC  X(01)     VALUE SPACES.
           03  WRK-DIGITO-R REDEFINES WRK-DIGITO PIC 9(01).
           03  WRK-TAMANHO             PIC  9(02)          VALUE 7.

       01  MENSAGEM                    PIC  X(50)     VALUE SPACES.

       01  LISTA-CAMPOS.
            03  DT-ENVIADA     PIC     9(08)  COMP-3.
            03  OPCAO          PIC     X(01)  VALUE 'H'.
            03  DT-JULIANA     PIC     9(07)  COMP-3.
            03  DT-EDITADA     PIC     X(10).
            03  DT-GREGORI     PIC     9(08).
            03  DIA-SEMANA     PIC     X(13).
            03  MES-EXTENSO    PIC     X(09).
            03  DIA-UT-ANTER   PIC     9(08).
            03  DIA-UT-POSTE   PIC     9(08).

       01  WRK-DT-PROC-N               PIC 9(08)   VALUE ZEROS.
       01  WRK-DT-PROC          REDEFINES   WRK-DT-PROC-N.
           03  WRK-DIA-PROC.
               05  WRK-DIA-PROC-N      PIC 9(02).
           03  WRK-MES-PROC.
               05  WRK-MES-PROC-N      PIC 9(02).
           03  WRK-ANO-PROC.
               05  WRK-ANO-PROC-N      PIC 9(04).

       01  WRK-DT-UTIL-N               PIC 9(08)   VALUE ZEROS.
       01  WRK-DT-UTIL          REDEFINES   WRK-DT-UTIL-N.
           03  WRK-DIA-UTIL.
               05  WRK-DIA-UTIL-N      PIC 9(02).
           03  WRK-MES-UTIL.
               05  WRK-MES-UTIL-N      PIC 9(02).
           03  WRK-ANO-UTIL.
               05  WRK-ANO-UTIL-N      PIC 9(04).

       01  WRK-DATA-AAAAMMDD1          PIC 9(08).
       01  FILLER          REDEFINES   WRK-DATA-AAAAMMDD1.
           03  WRK-ANO1                PIC 9(04).
           03  WRK-MES1                PIC 9(02).
           03  WRK-DIA1                PIC 9(02).

       01  WRK-DATA-AAAAMMDD2          PIC 9(08).
       01  FILLER          REDEFINES   WRK-DATA-AAAAMMDD2.
           03  WRK-ANO2                PIC 9(04).
           03  WRK-MES2                PIC 9(02).
           03  WRK-DIA2                PIC 9(02).

       01     WRK-CHAVE-4170.
         05   WRK-CONTRATO-4170        PIC  9(09)          VALUE ZEROS.
         05   WRK-FILLER               PIC  X(36)          VALUE SPACES.

       01     WRK-MENSAG-4170.
         05   WRK-FILLER-4170          PIC  X(13) VALUE 'BASE < DCOM >'.
         05   WRK-RESTO-4170           PIC  X(66) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM6416 ***'.
      *----------------------------------------------------------------*
       01  ENTRADA-6416.
           03  R6416E-COD-RETORNO      PIC  X(004).
           03  R6416E-MSG-RETORNO      PIC  X(079).
           03  R6416E-RESTART          PIC  9(005).
           03  R6416E-FLAG             PIC  X(001).
           03  R6416E-CFUNC-BDSCO      PIC  9(009).
           03  R6416E-CTERM            PIC  X(008).
           03  R6416E-BANCO            PIC  9(003).
           03  R6416E-DEPDC            PIC  9(005).

        01  SAIDA-6416.
            03  R6416S-HEADER.
                05  R6416S-COD-RETORNO  PIC  X(004).
                05  R6416S-MSG-RETORNO  PIC  X(079).
                05  R6416S-RESTART      PIC  9(005).
                05  R6416S-FLAG         PIC  X(001).
            03  R6416S-DADOS.
                05  R6416S-TABELA-SAIDA.
                    07  R6416S-AG-DEPDC            PIC 9(05).
                    07  R6416S-DG-DEPDC            PIC X(01).
                    07  R6416S-NOME-DEPDC          PIC X(40).
                    07  R6416S-END-DEPDC           PIC X(30).
                    07  R6416S-DIRETORIA-REG       PIC 9(05).
                    07  R6416S-DIRETORIA-NOME      PIC X(40).
                    07  R6416S-NOME-BANCO          PIC X(40).
                    07  R6416S-POLO-SERVICO        PIC 9(05).
                    07  R6416S-MUNIC-DEPDC         PIC X(25).
                    07  R6416S-UF                  PIC X(02).
                    07  R6416S-CEP                 PIC 9(05).
                    07  R6416S-CEP-COMPL           PIC 9(03).
                    07  R6416S-INDIC-DEPDC-AT      PIC X(01).
                    07  R6416S-NATUREZA-DEPDC      PIC X(01).
                    07  R6416S-COD-DISTR-CORR      PIC 9(05).
                    07  R6416S-DENCRR-DEPDC-BDSCO  PIC X(10).
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4170 ***'.
      *----------------------------------------------------------------*

       01  4170-ENTRADA.
           03  4170-ENT-COD-RETORNO          PIC  X(04).
           03  4170-ENT-MSG-RETORNO          PIC  X(79).
           03  4170-ENT-RESTART              PIC  9(05).
           03  4170-ENT-FLAG                 PIC  X(01).
           03  4170-ENT-START-COUNT          PIC  X(01).
           03  4170-ENT-CFUNC-BDSCO          PIC  9(09).
           03  4170-ENT-CTERM                PIC  X(08).
           03  4170-ENT-CCHAVE-ELMTO-DESC    PIC  X(45).
           03  4170-ENT-CINDCD-PSSOA-DESC    PIC  X(02).
           03  4170-ENT-CELMTO-DESC-COML     PIC  9(03).

       01  4170-SAIDA.
           03  4170-SAI-COD-RET              PIC  X(04).
           03  4170-SAI-COD-RET-9     REDEFINES 4170-SAI-COD-RET
                                             PIC  9(04).
           03  4170-SAI-MSG-RET              PIC  X(79).
           03  4170-SAI-RESTART              PIC  9(05).
           03  4170-SAI-FLAG                 PIC  X(01).
           03  4170-SAI-QTDE-RETORNADA       PIC  9(03).
           03  4170-SAI-COUNT                PIC  9(09).
           03  4170-SAI-DADOS.
FS2511*        05  4170-SAI-CCNPJ-CPF        PIC  9(09).
FS2511         05  4170-SAI-CCNPJ-CPF        PIC  X(09).
FS2511*        05  4170-SAI-CFLIAL-CNPJ      PIC  9(05).
FS2511         05  4170-SAI-CFLIAL-CNPJ      PIC  X(04).
               05  4170-SAI-CCTRL-CNPJ-CPF   PIC  9(02).
               05  4170-SAI-CDOCTO-ID        PIC  X(15).
               05  4170-SAI-IPSSOA-DESC-COML PIC  X(60).
               05  4170-SAI-CCLUB            PIC  9(10).
               05  4170-SAI-CBCO             PIC  9(03).
               05  4170-SAI-NOME-BANCO       PIC  X(40).
               05  4170-SAI-CAG-BCRIA        PIC  9(05).
               05  4170-SAI-NOME-AGENCIA     PIC  X(40).
               05  4170-SAI-CCTA-BCRIA-CLI   PIC  9(13).
               05  4170-SAI-CPOSTO-SERVC     PIC  9(03).
               05  4170-SAI-IPSSOA-CONJG     PIC  X(40).
               05  4170-SAI-CCPF-CONJG       PIC  9(09).
               05  4170-SAI-CCTRL-CPF-CONJG  PIC  9(02).
               05  4170-SAI-CDOCTO-ID-CONJG  PIC  X(15).
               05  4170-SAI-CINDCD-CONJG-AUTRZ
                                             PIC  X(01).
               05  4170-SAI-CCEP             PIC  9(05).
               05  4170-SAI-CCEP-COMPL       PIC  9(03).
               05  4170-SAI-ELOGDR           PIC  X(40).
               05  4170-SAI-CSGL-UF          PIC  X(02).

           03  4170-SAI-CONSISTENCIA REDEFINES 4170-SAI-DADOS.
               05  4170-SAI-TAMANHO-ERRO     PIC  9(01).
               05  4170-SAI-TABELA-ERROS     OCCURS 1 TIMES.
                   07  4170-SAI-CAMPOS-ERRO  PIC  9(01).

      *----------------------------------------------------------------*
      *             *** AREAS AUXILIARES PARA POOL5005 ***             *
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)    VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)    VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)    VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(004)    VALUE SPACES.
           05  WRK-TELA                PIC  X(008)    VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)    VALUE 'VRS002'.
           05  WRK-COD-USER            PIC  X(007)    VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).
           05  WRK-COD-DEPTO.
               10  FILLER              PIC  X(002)    VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)    VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)    VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)    VALUE SPACES.
               10  FILLER              PIC  X(1988)   VALUE SPACES.

      *---------------------------------------------------------------*
      *              *** AREA DE ERRO DA BRAD0660 *****
      *---------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)    VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)    VALUE
               '- RET.CODE ='.
           05  WRK-RETURN-CODE         PIC  9(002)    VALUE ZEROS.
           05  FILLER                  PIC  X(011)    VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)    VALUE ZEROS.
           05  FILLER                  PIC  X(002)    VALUE '*'.

      *---------------------------------------------------------------*
      *        *** AREA DA TELA DCOMDPTM  -  INPUT ***                *
      *---------------------------------------------------------------*

       01  INPUT-DPTM.
           05  FILLER                    PIC  X(018)    VALUE SPACES.
           05  INP-PFK                   PIC  X(001)    VALUE SPACES.
           05  INP-SENHAS                PIC  X(037)    VALUE SPACES.
           05  INP-COMANDO               PIC  X(068)    VALUE SPACES.
           05  INP-DADOS.
               10 INP-OPCAO              PIC  9(001)    VALUE ZEROS.
               10 INP-FASE               PIC  9(001)    VALUE ZEROS.
               10 INP-TRANSAC            PIC  X(008)    VALUE SPACES.
               10 INP-TMSTAMP            PIC  X(026)    VALUE SPACES.
               10 INP-TIPO               PIC  9(001)    VALUE ZEROS.
               10 INP-PAGINA             PIC  9(004)    VALUE ZEROS.
               10 INP-NOME               PIC  X(040)    VALUE SPACES.
               10 INP-CNPJCPF            PIC  X(019)    VALUE SPACES.
               10 INP-AGENC              PIC  X(028)    VALUE SPACES.
               10 INP-CONTA.
                  15 INP-CONTA-N         PIC  9(013)    VALUE ZEROS.
               10 INP-CDPROD.
                  15 INP-CDPROD-N        PIC  9(003)    VALUE ZEROS.
               10 INP-DSCPROD            PIC  X(012)    VALUE SPACES.
               10 INP-CONTRAT.
                  15 INP-CONTRAT-N       PIC  9(009)    VALUE ZEROS.
               10 INP-SPRODT1            PIC  X(050)    VALUE SPACES.
               10 INP-SPRODT2            PIC  X(050)    VALUE SPACES.
               10 INP-SPRODT3            PIC  X(050)    VALUE SPACES.
               10 INP-SITCONT            PIC  X(012)    VALUE SPACES.
               10 INP-DATCTR             PIC  X(010)    VALUE SPACES.
               10 INP-NRGARNT.
                  15 INP-NRGARNT-N       PIC  9(003)    VALUE ZEROS.
               10 INP-DSGARNT            PIC  X(020)    VALUE SPACES.
               10 INP-DATVCT             PIC  X(010)    VALUE SPACES.
               10 INP-PZODIAS.
                  15 INP-PZODIAS-N       PIC  9(003)    VALUE ZEROS.
               10 INP-VLRLIM.
                  15 INP-VLRLIM-N        PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
               10 INP-IRENAUT            PIC  X(001)    VALUE SPACES.
               10 INP-ISUBPRD            PIC  X(001)    VALUE SPACES.
               10 INP-ICTRLIM            PIC  X(001)    VALUE SPACES.
               10 INP-IISENTO            PIC  X(001)    VALUE SPACES.
               10 INP-IVLRFIX            PIC  X(001)    VALUE SPACES.
               10 INP-IPRCCTR            PIC  X(001)    VALUE SPACES.
               10 INP-VLTAC.
                  15 INP-VLTAC-N         PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
               10 INP-MENSAG             PIC  X(079)    VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DA TELA DCOMDPTM  -  OUTPUT ***                *
      *----------------------------------------------------------------*

       01  OUTPUT-DPTM.
           05  OUT-LL                    PIC S9(004) COMP VALUE +603.
           05  OUT-ZZ                    PIC S9(004) COMP VALUE +0.
           05  OUT-SENHAS                PIC  X(037)      VALUE SPACES.
           05  OUT-COMANDO               PIC  X(068)      VALUE SPACES.
           05  OUT-DADOS.
               10 OUT-OPCAO              PIC  9(001)    VALUE ZEROS.
               10 OUT-FASE               PIC  9(001)    VALUE ZEROS.
               10 OUT-TRANSAC            PIC  X(008)    VALUE SPACES.
               10 OUT-TMSTAMP            PIC  X(026)    VALUE SPACES.
               10 OUT-TIPO               PIC  9(001)    VALUE ZEROS.
               10 OUT-PAGINA             PIC  9(004)    VALUE ZEROS.
               10 OUT-NOME               PIC  X(040)    VALUE SPACES.
               10 OUT-CNPJCPF            PIC  X(019)    VALUE SPACES.
               10 OUT-AGENC              PIC  X(028)    VALUE SPACES.
               10 OUT-CONTA.
                  15 OUT-CONTA-N         PIC  9(013)    VALUE ZEROS.
               10 OUT-CDPROD.
                  15 OUT-CDPROD-N        PIC  9(003)    VALUE ZEROS.
               10 OUT-DSCPROD            PIC  X(012)    VALUE SPACES.
               10 OUT-CONTRAT.
                  15 OUT-CONTRAT-N       PIC  9(009)    VALUE ZEROS.
               10 OUT-SPRODT1            PIC  X(050)    VALUE SPACES.
               10 OUT-SPRODT2            PIC  X(050)    VALUE SPACES.
               10 OUT-SPRODT3            PIC  X(050)    VALUE SPACES.
               10 OUT-SITCONT            PIC  X(012)    VALUE SPACES.
               10 OUT-DATCTR             PIC  X(010)    VALUE SPACES.
               10 OUT-NRGARNT.
                  15 OUT-NRGARNT-N       PIC  9(003)    VALUE ZEROS.
               10 OUT-DSGARNT            PIC  X(020)    VALUE SPACES.
               10 OUT-DATVCT             PIC  X(010)    VALUE SPACES.
               10 OUT-PZODIAS.
                  15 OUT-PZODIAS-N       PIC  9(003)    VALUE ZEROS.
               10 OUT-VLRLIM.
                  15 OUT-VLRLIM-N        PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
               10 OUT-IRENAUT            PIC  X(001)    VALUE SPACES.
               10 OUT-ISUBPRD            PIC  X(001)    VALUE SPACES.
               10 OUT-ICTRLIM            PIC  X(001)    VALUE SPACES.
               10 OUT-IISENTO            PIC  X(001)    VALUE SPACES.
               10 OUT-IVLRFIX            PIC  X(001)    VALUE SPACES.
               10 OUT-IPRCCTR            PIC  X(001)    VALUE SPACES.
               10 OUT-VLTAC.
                  15 OUT-VLTAC-N         PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
           05  OUT-MENSAG                PIC  X(079)    VALUE SPACES.

      /---------------------------------------------------------------*
      *  AREA DA BRAD0660 COM MASCARA DA TELA DCOMDPTM - SEGMENTO 00  *
      *---------------------------------------------------------------*

       01  660-DCOMDPTM.
           05  660-LL-AREA             PIC  9(04)  COMP  VALUE 0070.
           05  660-LL-MENSAGEM         PIC  9(04)  COMP  VALUE 0603.
           05  660-SENHAS              PIC  9(04)  COMP  VALUE 0037.
           05  660-COMANDO             PIC  9(04)  COMP  VALUE 0068.
           05  660-OPCAO               PIC  9(04)  COMP  VALUE 0001.
           05  660-FASE                PIC  9(04)  COMP  VALUE 0001.
           05  660-TRANSAC             PIC  9(04)  COMP  VALUE 0008.
           05  660-TMSTAMP             PIC  9(04)  COMP  VALUE 0026.
           05  660-TIPO                PIC  9(04)  COMP  VALUE 0001.
           05  660-PAGINA              PIC  9(04)  COMP  VALUE 0004.
           05  660-NOME                PIC  9(04)  COMP  VALUE 0040.
           05  660-CNPJCPF             PIC  9(04)  COMP  VALUE 0019.
           05  660-AGENC               PIC  9(04)  COMP  VALUE 0028.
           05  660-CONTA               PIC  9(04)  COMP  VALUE 0013.
           05  660-CDPROD              PIC  9(04)  COMP  VALUE 0003.
           05  660-DSCPROD             PIC  9(04)  COMP  VALUE 0012.
           05  660-CONTRAT             PIC  9(04)  COMP  VALUE 0009.
           05  660-SPRODT1             PIC  9(04)  COMP  VALUE 0050.
           05  660-SPRODT2             PIC  9(04)  COMP  VALUE 0050.
           05  660-SPRODT3             PIC  9(04)  COMP  VALUE 0050.
           05  660-SITCONT             PIC  9(04)  COMP  VALUE 0012.
           05  660-DATCTR              PIC  9(04)  COMP  VALUE 0010.
           05  660-NRGARNT             PIC  9(04)  COMP  VALUE 0003.
           05  660-DSGARNT             PIC  9(04)  COMP  VALUE 0020.
           05  660-DATVCT              PIC  9(04)  COMP  VALUE 0010.
           05  660-PZODIAS             PIC  9(04)  COMP  VALUE 0003.
           05  660-VLRLIM              PIC  9(04)  COMP  VALUE 0018.
           05  660-IRENAUT             PIC  9(04)  COMP  VALUE 0001.
           05  660-ISUBPRD             PIC  9(04)  COMP  VALUE 0001.
           05  660-ICTRLIM             PIC  9(04)  COMP  VALUE 0001.
           05  660-IISENTO             PIC  9(04)  COMP  VALUE 0001.
           05  660-IVLRFIX             PIC  9(04)  COMP  VALUE 0001.
           05  660-IPRCCTR             PIC  9(04)  COMP  VALUE 0001.
           05  660-VLTAC               PIC  9(04)  COMP  VALUE 0018.
           05  660-MENSAG              PIC  9(04)  COMP  VALUE 0079.

      *---------------------------------------------------------------*
      *    AREA DE COMUNICACAO CHAMADO DCOM0413                       *
      *---------------------------------------------------------------*

       01  COMU-AREA-413.
           05  COMU-LL-413             PIC S9(004) COMP VALUE +160.
           05  COMU-ZZ-413             PIC S9(004) COMP VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-413  PIC  X(008)    VALUE SPACES.
               10  FILLER              PIC  X(007)    VALUE SPACES.
           05  COMU-SENHAS-413         PIC  X(037)    VALUE SPACES.
           05  COMU-COMANDO-413        PIC  X(068)    VALUE SPACES.
           05  COMU-DADOS-413.
               10  COMU-OPCAO-413      PIC  9(001)    VALUE ZEROS.
               10  COMU-TMSTAMP-413    PIC  X(026)    VALUE SPACES.
               10  COMU-OPERACAO-413   PIC  9(009)    VALUE ZEROS.

       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP  VALUE +272.
           05  COMU-ZZ                 PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(006)       VALUE SPACES.
               10  COMU-PFK            PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-OPCAO          PIC  9(001)       VALUE ZEROS.
               10  COMU-TIMESTAMP      PIC  X(026)       VALUE SPACES.
               10  COMU-CONTRATO       PIC  9(009)       VALUE ZEROS.
               10  COMU-AGENCIA        PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA          PIC  9(013)       VALUE ZEROS.
FS2511*        10  COMU-CPF            PIC  9(009)       VALUE ZEROS.
FS2511         10  COMU-CPF            PIC  X(009)       VALUE SPACES.
FS2511*        10  COMU-FILIAL         PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL         PIC  X(04)        VALUE SPACES.
               10  COMU-DIGITO         PIC  9(002)       VALUE ZEROS.
               10  COMU-MENSAG         PIC  X(079)       VALUE SPACES.

      *---------------------------------------------------------------
      *        *** AREA DE COMUNICACAO CHAMADO DCOM0414
      *---------------------------------------------------------------

       01  COMU-AREA-414.
           05  COMU-LL-414            PIC S9(004) COMP    VALUE +244.
           05  COMU-ZZ-414            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-414.
               10  COMU-TRANSACAO-414 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(007)         VALUE SPACES.
           05  COMU-SENHAS-414        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-414       PIC  X(068)         VALUE SPACES.
           05  COMU-OPCAO-414         PIC  9(001)         VALUE ZEROS.
           05  COMU-TMSTAMP-414       PIC  X(026)         VALUE SPACES.
           05  COMU-CONTRATO-414      PIC  9(009)         VALUE ZEROS.
           05  COMU-CODAGE-414        PIC  9(005)         VALUE ZEROS.
           05  COMU-MENSA-414         PIC  X(079)         VALUE SPACES.

       01  COMU-AREA-415.
           05  COMU-LL-415            PIC S9(004) COMP    VALUE +276.
           05  COMU-ZZ-415            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-415.
               10  COMU-TRANSACAO-415 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(007)         VALUE SPACES.
           05  COMU-SENHAS-415        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-415       PIC  X(068)         VALUE SPACES.
           05  COMU-OPCAO-415         PIC  9(001)         VALUE ZEROS.
           05  COMU-TMSTAMP-415       PIC  X(026)         VALUE SPACES.
           05  COMU-CONTRATO-415      PIC  9(009)         VALUE ZEROS.
           05  COMU-CODAGE-415-X.
               10  COMU-CODAGE-415-N  PIC  9(005)         VALUE ZEROS.
           05  COMU-CONTA-415-X.
               10  COMU-CONTA-415-N   PIC  9(013)         VALUE ZEROS.
           05  COMU-NUMERO-415-X.
FS2511*        10  COMU-NUMERO-415-N  PIC  9(009)         VALUE ZEROS.
FS2511         10  COMU-NUMERO-415-N  PIC  X(009)         VALUE SPACES.
           05  COMU-FILIAL-415-X.
FS2511*        10  COMU-FILIAL-415-N  PIC  9(004)         VALUE ZEROS.
FS2511         10  COMU-FILIAL-415-N  PIC  X(04)          VALUE SPACES.
           05  COMU-CONTROLE-415-X.
               10  COMU-CONTROLE-415-N PIC 9(002)         VALUE ZEROS.
           05  COMU-PAGINA-415         PIC 9(004)         VALUE ZEROS.
           05  COMU-MENSA-415          PIC X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO PFK ***                         *
      *----------------------------------------------------------------*

       01  COMU-AREA-PFK.
           05  COMU-LL-PFK             PIC S9(004) COMP    VALUE +124.
           05  COMU-ZZ-PFK             PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-PFK  PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK1           PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-PFK         PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-PFK        PIC  X(068)         VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DA BRAD7100 ***                                *
      *----------------------------------------------------------------*

       COPY  'I#BRAD7C'.

       COPY  'I#DCOMSY'.

       01  WRK-4001E-ENTRADA.
           03  WRK-4001E-COD-RETORNO      PIC  X(004).
           03  WRK-4001E-MSG-RETORNO      PIC  X(079).
           03  WRK-4001E-RESTART          PIC  9(005).
           03  WRK-4001E-FLAG             PIC  X(001).
           03  WRK-4001E-CFUNC-BDSCO      PIC  9(009).
           03  WRK-4001E-CTERM            PIC  X(008).
           03  WRK-4001E-CPRODT           PIC  9(003).
           03  WRK-4001E-DT-MOVTO         PIC  X(010).

       01  WRK-4001S-SAIDA.
           03  WRK-4001S-HEADER.
               05  WRK-4001S-COD-RETORNO               PIC  X(004).
               05  WRK-4001S-MSG-RETORNO               PIC  X(079).
               05  WRK-4001S-RESTART                   PIC  9(005).
               05  WRK-4001S-FLAG                      PIC  X(001).
           03  WRK-4001S-DADOS.
               05  WRK-4001S-TABELA-SAIDA.
                   07  WRK-4001S-CPRODT                PIC  9(003).
                   07  WRK-4001S-IPRODT                PIC  X(060).
                   07  WRK-4001S-IABREV-PRODT          PIC  X(012).
                   07  WRK-4001S-CSGL-PRODT-DESC       PIC  X(006).
                   07  WRK-4001S-CIDTFD-TX-MIN-JURO    PIC  9(005).
                   07  WRK-4001S-CIDTFD-TX-MIN-VLR     PIC  9(015).
                   07  WRK-4001S-CIDTFD-TX-MIN-DEC     PIC  9(003).
                   07  WRK-4001S-CIDTFD-TX-MAX-JURO    PIC  9(005).
                   07  WRK-4001S-CIDTFD-TX-MAX-VLR     PIC  9(015).
                   07  WRK-4001S-CIDTFD-TX-MAX-DEC     PIC  9(003).
                   07  WRK-4001S-HULT-ATULZ            PIC  X(022).
           03  WRK-4001S-CONSISTENCIA     REDEFINES
               WRK-4001S-DADOS.
               05  WRK-4001S-TAMANHO-ERRO              PIC  9(001).
               05  WRK-4001S-TABELA-ERROS OCCURS  002  TIMES.
                   07  WRK-4001S-CAMPOS-ERRO           PIC  9(001).


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4128 ***'.
      *----------------------------------------------------------------*

       01  WRK-4128E-ENTRADA.
           03  WRK-4128E-COD-RETORNO            PIC  X(004).
           03  WRK-4128E-MSG-RETORNO            PIC  X(079).
           03  WRK-4128E-RESTART                PIC  9(005).
           03  WRK-4128E-FLAG                   PIC  X(001).
           03  WRK-4128E-CFUNC-BDSCO            PIC  9(009).
           03  WRK-4128E-CTERM                  PIC  X(008).
           03  WRK-4128E-CCONTR-LIM-DESC        PIC  9(009).
           03  WRK-4128E-CVRSAO-CONTR-LIM       PIC  9(003).
           03  WRK-4128E-FLAG-FILTRO            PIC  X(001).

       01  WRK-4128S-SAIDA.
           03  WRK-4128S-HEADER.
               05  WRK-4128S-COD-RETORNO        PIC  X(004).
               05  WRK-4128S-MSG-RETORNO        PIC  X(079).
               05  WRK-4128S-RESTART            PIC  9(005).
               05  WRK-4128S-FLAG               PIC  X(001).
           03  WRK-4128S-DADOS.
               05  WRK-4128S-CPRODT             PIC  9(003).
               05  WRK-4128S-ISIT-DESC-COML     PIC  X(030).
               05  WRK-4128S-IRSUMO-SIT-DESC    PIC  X(015).
FS2511*        05  WRK-4128S-CCNPJ-CPF          PIC  9(009).
FS2511         05  WRK-4128S-CCNPJ-CPF          PIC  X(009).
FS2511*        05  WRK-4128S-CFLIAL-CNPJ        PIC  9(004).
FS2511         05  WRK-4128S-CFLIAL-CNPJ        PIC  X(04).
               05  WRK-4128S-CCTRL-CNPJ         PIC  9(002).
               05  WRK-4128S-IPSSOA-DESC-COML   PIC  X(060).
               05  WRK-4128S-CBCO               PIC  9(003).
               05  WRK-4128S-CBCO-DESC          PIC  X(020).
               05  WRK-4128S-CAG-BCRIA          PIC  9(005).
               05  WRK-4128S-CAG-BCRIA-DESC     PIC  X(030).
               05  WRK-4128S-CCTA-BCRIA-CLI     PIC  9(013).
               05  WRK-4128S-DINIC-VGCIA-CONTR  PIC  X(010).
               05  WRK-4128S-CINDCD-RENOV-AUTOM PIC  X(001).
               05  WRK-4128S-DVCTO-CONTR-LIM    PIC  X(010).
               05  WRK-4128S-TCONTR-LIM-DESC    PIC  9(005).
               05  WRK-4128S-VCONTR-LIM-DESC    PIC  9(015)V99.
               05  WRK-4128S-VDISPN-CONTR-LIM   PIC  9(015)V99.
               05  WRK-4128S-CTPO-COBR-TAC      PIC  9(003).
               05  WRK-4128S-ITPO-COBR-TAC      PIC  X(030).
               05  WRK-4128S-VTAC-CONTR-LIM     PIC  9(015)V99.

               05  WRK-4128S-PARECER            PIC  X(240).
               05  WRK-4128S-CGARAN             PIC  9(003).
               05  WRK-4128S-IGARAN             PIC  X(040).
               05  WRK-4128S-VUTILZ-LIM-DESC    PIC  9(15)V99.

           03  WRK-4128S-CONSISTENCIA REDEFINES WRK-4128S-DADOS.
               05  WRK-4128S-TAMANHO-ERRO       PIC  9(001).
               05  WRK-4128S-TABELA-ERROS       OCCURS 01 TIMES.
                   07  WRK-4128S-CAMPOS-ERRO    PIC  9(001).


       01  WRK-4128S-CSPROD-DESC-COML     PIC  9(003) VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4113 ***'.
      *----------------------------------------------------------------*

       01  ENTRADA-ROTEADOR-4113.
           05  WRK-HEADER-4113.
             10  ROTENT-COD-RETORNO-4113     PIC  X(004).
             10  ROTENT-MSG-RETORNO-4113     PIC  X(079).
             10  ROTENT-RESTART-4113         PIC  9(005).
             10  ROTENT-FLAG-4113            PIC  9(001).
             10  ROTENT-START-COUNT-4113     PIC  X(001).
             10  ROTENT-CFUNC-BDSCO-4113     PIC  9(009).
             10  ROTENT-CTERM-4113           PIC  X(008).
           05  ROTENT-DADOS-4113.
             10  ROTENT-CPRODT-4113          PIC  9(003).
             10  ROTENT-CCONTR-LIM-DESC-4113 PIC  9(009).

       01  SAIDA-ROTEADOR-4113.
           05  ROTSAI-HEADER-4113.
             10  ROTSAI-COD-RETORNO-4113      PIC  X(004).
             10  ROTSAI-MSG-RETORNO-4113      PIC  X(079).
             10  ROTSAI-RESTART-4113          PIC  9(005).
             10  ROTSAI-FLAG-4113             PIC  X(001).
             10  ROTSAI-QTDE-RETORNADA-4113   PIC  9(003).
             10  ROTSAI-COUNT-4113            PIC  9(009).

           05  ROTSAI-DADOS-4113.
             10  ROTSAI-CPRODT-4113           PIC  9(003).
             10  ROTSAI-IPRODT-4113           PIC  X(060).
             10  ROTSAI-IABREV-PRODT-4113     PIC  X(012).
             10  ROTSAI-CSGL-PRODT-4113       PIC  X(006).
             10  ROTSAI-TABELA-SAIDA-4113  OCCURS 10  TIMES.
                 15  ROTSAI-CSPROD-4113   PIC 9(003).
                 15  ROTSAI-ISPROD-4113   PIC X(040).
                 15  ROTSAI-IRSUMO-SPROD-4113       PIC X(010).
                 15  ROTSAI-CSGL-SPROD-4113         PIC X(006).

           05  ROTSAI-CONSISTENCIA-4113  REDEFINES  ROTSAI-DADOS-4113.
             10  ROTSAI-TAMANHO-ERRO-4113     PIC  9(001).
             10  ROTSAI-TABELA-ERROS-4113  OCCURS  2 TIMES.
                 15  ROTSAI-CAMPOS-ERRO-4113  PIC  9(001).

       01  WRK-ERRO-AREA-4113                   PIC  X(107).
       01  WRK-SQLCA-4113                       PIC  X(136).

      *----------------------------------------------------------------*
      *** AREA DE COMUNICACAO DCOM5127 ***                             *
      *----------------------------------------------------------------*
       01  WRK-5127E-ENTRADA.
          03  WRK-5127E-COD-RETORNO    PIC  X(04).
          03  WRK-5127E-MSG-RETORNO    PIC  X(79).
          03  WRK-5127E-RESTART        PIC  9(05).
          03  WRK-5127E-FLAG           PIC  X(01).
          03  WRK-5127E-CFUNC-BDSCO    PIC  9(09).
          03  WRK-5127E-CTERM          PIC  X(08).
          03  WRK-5127E-CPRODT         PIC  9(03).
          03  WRK-5127E-CCONTR-LIM-DESC
                                       PIC  9(09).

       01  WRK-5127S-SAIDA.
          03  WRK-5127S-COD-RETORNO    PIC  X(04).
          03  WRK-5127S-MSG-RETORNO    PIC  X(79).
          03  WRK-5127S-RESTART        PIC  9(05).
          03  WRK-5127S-FLAG           PIC  X(01).
          03  WRK-5127S-COUNT          PIC  9(09).
          03  WRK-5127S-CONSISTENCIA.
              05  WRK-5127S-TAMANHO-ERRO
                                       PIC  9(01).
              05  WRK-5127S-TABELA-ERROS    OCCURS 57 TIMES.
                  07  WRK-5127S-CAMPOS-ERRO
                                       PIC  9(01).

      *----------------------------------------------------------------*
      *** AREA DE COMUNICACAO DCOM5142 ***                             *
      *----------------------------------------------------------------*

       01  ENTRADA-ROTEADOR.
           03  ROTENT-COD-RETORNO                  PIC  X(04).
           03  ROTENT-MSG-RETORNO                  PIC  X(79).
           03  ROTENT-RESTART                      PIC  9(05).
           03  ROTENT-FLAG                         PIC  X(01).
           03  ROTENT-CFUNC-BDSCO                  PIC  9(09).
           03  ROTENT-CTERM                        PIC  X(08).
           03  ROTENT-CCONTR-LIM-DESC              PIC  9(09).

       01  SAIDA-ROTEADOR.
           03  ROTSAI-HEADER.
               05  ROTSAI-COD-RETORNO              PIC  X(04).
               05  ROTSAI-MSG-RETORNO              PIC  X(79).
               05  ROTSAI-RESTART                  PIC  9(05).
               05  ROTSAI-FLAG                     PIC  X(01).
               05  ROTSAI-COUNT                    PIC  9(09).

       01  WRK-AUXILIAR-TELA.
           05  WRK-LIN-SUBPROD.
               10  WRK-LIN-SUBP-NRO1    PIC  X(003)     VALUE SPACES.
               10  TRACO-1              PIC  X(1)       VALUE SPACES.
               10  WRK-LIN-SUBP-DESC1   PIC  X(006)     VALUE SPACES.
               10  BARRA-1              PIC  X(3)       VALUE SPACES.
               10  WRK-LIN-SUBP-NRO2    PIC  X(003)     VALUE SPACES.
               10  TRACO-2              PIC  X(1)       VALUE SPACES.
               10  WRK-LIN-SUBP-DESC2   PIC  X(006)     VALUE SPACES.
               10  BARRA-2              PIC  X(3)       VALUE SPACES.
               10  WRK-LIN-SUBP-NRO3    PIC  X(003)     VALUE SPACES.
               10  TRACO-3              PIC  X(1)       VALUE SPACES.
               10  WRK-LIN-SUBP-DESC3   PIC  X(006)     VALUE SPACES.
               10  BARRA-3              PIC  X(3)       VALUE SPACES.
               10  WRK-LIN-SUBP-NRO4    PIC  X(003)     VALUE SPACES.
               10  TRACO-4              PIC  X(1)       VALUE SPACES.
               10  WRK-LIN-SUBP-DESC4   PIC  X(006)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM6408 ***'.
      *----------------------------------------------------------------*
           COPY  'I#DCOMX3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM6437 ***'.
      *----------------------------------------------------------------*
           COPY 'I#DCOMXE'.

      *----------------------------------------------------------------*
      *        *** DCOM0416 - FIM DA AREA DE WORKING ***               *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING  LNK-IO-PCB
                                              LNK-ALT-PCB.

           MOVE  WRK-GU                TO     WRK-FUNCAO.

      *----------------------------------------------------------------*
      *        PROCEDIMENTOS INICIAIS                                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL  'POOL5005'            USING  WRK-VERSAO
                                              WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              WRK-MENSAGEM
                                              WRK-TELA
                                              WRK-COD-USER
                                              WRK-COD-DEPTO.

           IF RETURN-CODE    EQUAL  04  OR  12
              GOBACK
           END-IF.

           MOVE SPACES          TO  OUT-DADOS.

           PERFORM  1000-PROCESSAR-MENSAGEM.

           GO   TO  0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE  'DCOMDPTM'             TO WRK-TELA.

           IF WRK-TRANSACAO            NOT EQUAL  'DCOM0416'
              PERFORM  2100-RECEBER-CONTROLE
           ELSE
              PERFORM  2200-PROCESSAR-MENSAGEM
           END-IF.

           IF WRK-TELA                 EQUAL  'DCOMDPTM'
              PERFORM  3000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     WRK-4128E-ENTRADA
                                          WRK-4128S-SAIDA
                                          ENTRADA-ROTEADOR-4113
                                          SAIDA-ROTEADOR-4113.

           IF WRK-TRANSACAO            EQUAL 'DCOM0413'
              MOVE WRK-MENSAGEM           TO COMU-AREA-413
              MOVE COMU-TRANSACAO-413     TO INP-TRANSAC
                                             OUT-TRANSAC
              MOVE COMU-OPCAO-413         TO INP-OPCAO
                                             OUT-OPCAO
              MOVE COMU-TMSTAMP-413       TO INP-TMSTAMP
                                             OUT-TMSTAMP
              MOVE COMU-OPERACAO-413      TO INP-CONTRAT
                                             OUT-CONTRAT
                                             WRK-4128E-CCONTR-LIM-DESC
                                             ROTENT-CCONTR-LIM-DESC-4113
                                             WRK-CONTRATO-4170
           ELSE
            IF WRK-TRANSACAO           EQUAL 'DCOM0414'
               MOVE WRK-MENSAGEM          TO COMU-AREA-414
               MOVE COMU-TRANSACAO-414    TO INP-TRANSAC
                                             OUT-TRANSAC
               MOVE COMU-OPCAO-414        TO INP-OPCAO
                                             OUT-OPCAO
               MOVE COMU-TMSTAMP-414      TO INP-TMSTAMP
                                             OUT-TMSTAMP
               MOVE COMU-CONTRATO-414     TO INP-CONTRAT
                                             OUT-CONTRAT
                                             WRK-4128E-CCONTR-LIM-DESC
                                             ROTENT-CCONTR-LIM-DESC-4113
                                             WRK-CONTRATO-4170
            ELSE
            IF WRK-TRANSACAO           EQUAL 'DCOM0415'
               MOVE WRK-MENSAGEM          TO COMU-AREA-415
               MOVE COMU-TRANSACAO-415    TO INP-TRANSAC
                                             OUT-TRANSAC
               MOVE COMU-OPCAO-415        TO INP-OPCAO
                                             OUT-OPCAO
               MOVE COMU-TMSTAMP-415      TO INP-TMSTAMP
                                             OUT-TMSTAMP
               MOVE COMU-CONTRATO-415     TO INP-CONTRAT
                                             OUT-CONTRAT
                                             WRK-4128E-CCONTR-LIM-DESC
                                             ROTENT-CCONTR-LIM-DESC-4113
                                             WRK-CONTRATO-4170
               MOVE COMU-PAGINA-415       TO INP-PAGINA
                                             OUT-PAGINA

               IF   COMU-CODAGE-415-N     GREATER  ZEROS
                    MOVE 1                TO INP-TIPO  OUT-TIPO
               END-IF
            END-IF
            END-IF
           END-IF.

           MOVE 'N'                   TO WRK-ERRO-ACESSO.

           PERFORM 2110-ACESSAR-DCOM4128.
           IF  WRK-ERRO-ACESSO        EQUAL 'S'
               GO TO 2100-99-FIM
           END-IF

           PERFORM 2299-VALIDAR-PERFIL.

           IF  WRK-INCONSIS           EQUAL '*'
               MOVE SG-MENSAGEM(0204)      TO OUT-MENSAG
               MOVE 'S'                    TO WRK-EFETUADO
                                              WRK-PERFIL
               PERFORM 2220-RETORNAR-CHAMADOR
               GO TO 2100-99-FIM
           END-IF.

           IF  WRK-ERRO-ACESSO        EQUAL  'N'
               PERFORM 2130-ACESSAR-DCOM4113
           END-IF.

           IF  WRK-ERRO-ACESSO        EQUAL  'N'
               MOVE  'TECLE <PF6> PARA EFETIVAR A LIBERACAO OU <PF3> PAR
      -        'A RETORNAR'           TO  OUT-MENSAG
           END-IF.

           MOVE  ZEROS                TO  INP-FASE  OUT-FASE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-ACESSAR-DCOM4128           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-4128E-RESTART
           MOVE SPACES                 TO WRK-4128E-FLAG
           MOVE WRK-COD-USER           TO WRK-4128E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4128E-CTERM
           MOVE 'L'                    TO WRK-4128E-FLAG-FILTRO
           MOVE 'DCOM4128'             TO WRK-MODULO
           MOVE 'N'                    TO WRK-ERRO-ACESSO

           CALL WRK-MODULO    USING    WRK-4128E-ENTRADA
                                       WRK-4128S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA-416.

           EVALUATE WRK-4128S-COD-RETORNO
               WHEN  '0000'
                   MOVE 'N'                   TO WRK-ERRO-ACESSO
                   MOVE WRK-4128S-CCNPJ-CPF(1:3)   TO WRK-CNPJ1
                   MOVE WRK-4128S-CCNPJ-CPF(4:3)   TO WRK-CNPJ2
                   MOVE WRK-4128S-CCNPJ-CPF(7:3)   TO WRK-CNPJ3
                   MOVE WRK-4128S-CFLIAL-CNPJ TO WRK-FILIAL
                   MOVE WRK-4128S-CCTRL-CNPJ  TO WRK-CONTROLE
                   MOVE WRK-CNPJ-COMPL        TO OUT-CNPJCPF
                   MOVE WRK-4128S-CAG-BCRIA   TO WRK-AGENCIA

                   PERFORM 2111-BUSCA-NOME-AGENCIA
                   MOVE WRK-AGENC-COMPL       TO OUT-AGENC

                   MOVE WRK-4128S-CCTA-BCRIA-CLI TO OUT-CONTA
                                                    WRK-CONTA

                   PERFORM  2112-CALCULAR-DIGITO

                   INSPECT WRK-4128S-DINIC-VGCIA-CONTR
                                            REPLACING ALL '.' BY '/'
                   INSPECT WRK-4128S-DVCTO-CONTR-LIM
                                            REPLACING ALL '.' BY '/'

                   MOVE WRK-4128S-DINIC-VGCIA-CONTR
                                       TO INP-DATCTR
                                          OUT-DATCTR
                   MOVE WRK-4128S-DVCTO-CONTR-LIM
                                       TO INP-DATVCT
                                          OUT-DATVCT
                   MOVE WRK-4128S-TCONTR-LIM-DESC
                                       TO OUT-PZODIAS-N
                   MOVE WRK-4128S-VCONTR-LIM-DESC
                                       TO OUT-VLRLIM-N
                   MOVE WRK-4128S-CINDCD-RENOV-AUTOM
                                       TO INP-IRENAUT
                                          OUT-IRENAUT
                   PERFORM 2125-VERIFICA-TIPO-TAC

                   MOVE WRK-4128S-VTAC-CONTR-LIM
                                       TO INP-VLTAC-N
                                          OUT-VLTAC-N

                   MOVE WRK-4128S-CPRODT   TO INP-CDPROD-N
                                          OUT-CDPROD-N
                   PERFORM 2127-ACESSAR-DCOM4001

                   IF  WRK-ERRO-ACESSO       EQUAL 'N'
                       PERFORM  2117-ACESSAR-DCOM4170
                       IF  4170-SAI-CCLUB   GREATER ZEROS
                           PERFORM 2113-ACESSAR-DCOM6408
                       END-IF
                   END-IF

                   MOVE  WRK-4128S-IRSUMO-SIT-DESC TO OUT-SITCONT
                   MOVE  WRK-4128S-CGARAN          TO OUT-NRGARNT-N
                   MOVE  WRK-4128S-IGARAN          TO OUT-DSGARNT

               WHEN '0003'
                   INITIALIZE COMU-AREA
                   MOVE 'S'                   TO WRK-ERRO-ACESSO
                   MOVE SG-MENSAGEM(0197)
                                       TO COMU-MENSAG
                   PERFORM 2115-RETORNA-CHAMADOR-DCOM0413

               WHEN OTHER
                   INITIALIZE COMU-AREA
                   MOVE 'S'                   TO WRK-ERRO-ACESSO
                   MOVE WRK-4128S-MSG-RETORNO
                                       TO COMU-MENSAG
                   PERFORM 2115-RETORNA-CHAMADOR-DCOM0413

           END-EVALUATE.
      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2299-VALIDAR-PERFIL             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO.

           MOVE WRK-COD-USER           TO ROTENT-6437-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6437-CTERM.
           MOVE WRK-COD-DEPTO-N        TO ROTENT-6437-AGEUSU-ENV.
           MOVE WRK-4128S-CAG-BCRIA    TO ROTENT-6437-AGEPESQ-ENV
           MOVE 'A'                    TO ROTENT-6437-FUNCAO-ENV.
           MOVE 'DCOM6437'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6437-ENVIO
                                             ROTSAI-6437-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET     NOT EQUAL ZEROS OR
               ROTSAI-6437-RESUL-RET   GREATER 3
               MOVE '*'            TO WRK-INCONSIS
           END-IF.

      *----------------------------------------------------------------*
       2299-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2111-BUSCA-NOME-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO R6416E-COD-RETORNO.
           MOVE SPACES                 TO R6416E-MSG-RETORNO.
           MOVE ZEROS                  TO R6416E-RESTART.
           MOVE 'N'                    TO R6416E-FLAG.
           MOVE WRK-COD-USER           TO R6416E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO R6416E-CTERM.
           MOVE WRK-4128S-CBCO         TO R6416E-BANCO.
           MOVE WRK-4128S-CAG-BCRIA    TO R6416E-DEPDC.
           MOVE 'DCOM6416'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ENTRADA-6416
                                             SAIDA-6416
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF R6416S-COD-RETORNO       NOT EQUAL ZEROS
              MOVE 'AGENCIA NAO ENCONTRADA' TO WRK-DESC-AG
           ELSE
              MOVE R6416S-NOME-DEPDC        TO WRK-DESC-AG
           END-IF.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2112-CALCULAR-DIGITO          SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0431'   USING WRK-CONTA
                                   WRK-DIGITO
                                   WRK-TAMANHO.


           IF  WRK-DIGITO       EQUAL  '.'
               MOVE 'APL'          TO ERR-TIPO-ACESSO
               MOVE 'ERRO NO CALCULO DO DIGITO - BRAD0431 - CONTA = '
                                   TO ERR-TEXTO(1:47)
               MOVE WRK-CONTA           TO WRK-CONTA-R
               MOVE WRK-CONTA-R         TO ERR-TEXTO(48:13)
                   MOVE '2112-CALCULAR-DIGITO-ERRO'
                                        TO OUT-MENSAG
           ELSE
               IF  WRK-DIGITO           EQUAL  'P'
                   MOVE ZEROS           TO WRK-DIGITO-R
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2113-ACESSAR-DCOM6408           SECTION.
      *----------------------------------------------------------------*
      *                    BUSCA NOME DO CLIENTE
      *----------------------------------------------------------------*
           INITIALIZE    ROTENT-6408-ENVIO.

           MOVE   ZEROS                TO  ROTENT-6408-COD-ENV
                                           ROTENT-6408-REST-ENV

           MOVE   SPACES               TO  ROTENT-6408-MSG-ENV
           MOVE   'N'                  TO  ROTENT-6408-FLAG-ENV
                                           ROTENT-6408-START-ENV

           MOVE  WRK-COD-USER          TO ROTENT-6408-CFUNC-ENV
           MOVE  LNK-IO-LTERM          TO ROTENT-6408-CTERM-ENV
           MOVE  4170-SAI-CCLUB        TO ROTENT-6408-CCLUB-ENV
           MOVE  WRK-4128S-CAG-BCRIA   TO ROTENT-6408-CJUNC-ENV

           MOVE  WRK-4128S-CCTA-BCRIA-CLI  TO ROTENT-6408-CCTAC-ENV
           MOVE  WRK-DIGITO-R              TO ROTENT-6408-DCTAC-ENV

           MOVE  ZEROS                 TO ROTENT-6408-NUM-CNPJ-ENV
                                          ROTENT-6408-FIL-CNPJ-ENV
                                          ROTENT-6408-CTR-CNPJ-ENV
                                          ROTENT-6408-CCLUB-ENV.

           MOVE 'DCOM6408'             TO WRK-MODULO.

           CALL WRK-MODULO  USING ROTENT-6408-AREA
                                  ROTSAI-6408-RETORNO
                                  ERRO-AREA
                                  WRK-SQLCA-416.

           EVALUATE ROTSAI-6408-COD-RET

               WHEN '0000'

                   MOVE 'N'            TO WRK-ERRO-ACESSO
                   MOVE ROTSAI-6408-NM-RZSOC-RET(1)
                                       TO OUT-NOME

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   MOVE '0020'         TO ERR-LOCAL
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE 'S'            TO WRK-ERRO-ACESSO
                   MOVE ROTSAI-6408-MENS-RET
                                       TO OUT-MENSAG
           END-EVALUATE.

      *----------------------------------------------------------------*
       2113-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2115-RETORNA-CHAMADOR-DCOM0413  SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA     TO COMU-LL
           MOVE WRK-CHNG             TO WRK-FUNCAO
           MOVE 'DCOM0413'           TO WRK-TELA
           MOVE 'DCOM0416'           TO COMU-TRANSACAO
           MOVE INP-OPCAO            TO COMU-OPCAO
           MOVE INP-TMSTAMP          TO COMU-TIMESTAMP
           MOVE COMU-AREA            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2115-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2117-ACESSAR-DCOM4170           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      4170-ENTRADA
                                           4170-SAIDA.

           MOVE   WRK-CHAVE-4170       TO  4170-ENT-CCHAVE-ELMTO-DESC
           MOVE  'CP'                  TO  4170-ENT-CINDCD-PSSOA-DESC
           MOVE  'S'                   TO  WRK-ERRO-ACESSO
           MOVE 'DCOM4170'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4170-ENTRADA
                                             4170-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4170-SAI-COD-RET
               WHEN  '0000'

                   MOVE 'N'            TO WRK-ERRO-ACESSO
                   MOVE 4170-SAI-IPSSOA-DESC-COML TO OUT-NOME
               WHEN '0099'

                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   MOVE '0010'         TO ERR-LOCAL
                   MOVE '2132-ACESSAR-DCOM4170'
                                       TO ERR-TEXTO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                  MOVE 'S'                       TO WRK-ERRO-ACESSO
                  MOVE 4170-SAI-MSG-RET          TO  WRK-RESTO-4170
                  MOVE WRK-MENSAG-4170           TO  OUT-MENSAG

           END-EVALUATE.

      *----------------------------------------------------------------*
       2117-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2125-VERIFICA-TIPO-TAC           SECTION.
      *----------------------------------------------------------------*

           EVALUATE  TRUE
               WHEN  WRK-4128S-CTPO-COBR-TAC     EQUAL  1
                     MOVE ' '               TO  INP-ISUBPRD
                                                OUT-ISUBPRD
                     MOVE 'X'               TO  INP-ICTRLIM
                                                OUT-ICTRLIM
                     MOVE 'X'               TO  INP-IISENTO
                                                OUT-IISENTO

               WHEN  WRK-4128S-CTPO-COBR-TAC     EQUAL  2
                     MOVE ' '               TO  INP-ISUBPRD
                                                OUT-ISUBPRD
                     MOVE 'X'               TO  INP-ICTRLIM
                                                OUT-ICTRLIM
                     MOVE 'X'               TO  INP-IVLRFIX
                                                OUT-IVLRFIX

               WHEN  WRK-4128S-CTPO-COBR-TAC     EQUAL  3
                     MOVE ' '               TO  INP-ISUBPRD
                                                OUT-ISUBPRD
                     MOVE 'X'               TO  INP-ICTRLIM
                                                OUT-ICTRLIM
                     MOVE 'X'               TO  INP-IPRCCTR
                                                OUT-IPRCCTR

               WHEN  OTHER
                     MOVE 'X'               TO  INP-ISUBPRD
                                                OUT-ISUBPRD
           END-EVALUATE.

      *----------------------------------------------------------------*
       2125-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2127-ACESSAR-DCOM4001           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     WRK-4001E-ENTRADA
                                          WRK-4001S-SAIDA

           MOVE ZEROS                  TO WRK-4001E-RESTART
           MOVE SPACES                 TO WRK-4001E-FLAG
           MOVE WRK-COD-USER           TO WRK-4001E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4001E-CTERM
           MOVE WRK-4128S-CPRODT       TO WRK-4001E-CPRODT
           MOVE INP-DATCTR             TO WRK-4001E-DT-MOVTO
           INSPECT WRK-4001E-DT-MOVTO  REPLACING ALL '/' BY '.'
           MOVE 'DCOM4001'             TO WRK-MODULO
           MOVE 'S'                    TO WRK-ERRO-ACESSO

           CALL WRK-MODULO    USING    WRK-4001E-ENTRADA
                                       WRK-4001S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA-416.

           EVALUATE WRK-4001S-COD-RETORNO
               WHEN  '0000'

                   MOVE 'N'            TO WRK-ERRO-ACESSO
                   MOVE WRK-4001S-IABREV-PRODT
                                       TO INP-DSCPROD
                                          OUT-DSCPROD

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   MOVE '0050'         TO ERR-LOCAL
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE WRK-4001S-MSG-RETORNO
                                       TO OUT-MENSAG

           END-EVALUATE.

      *----------------------------------------------------------------*
       2127-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2130-ACESSAR-DCOM4113           SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-COD-USER          TO ROTENT-CFUNC-BDSCO-4113
           MOVE  LNK-IO-LTERM          TO ROTENT-CTERM-4113
           MOVE  ZEROS                 TO ROTENT-RESTART-4113
           MOVE  ZEROS                 TO ROTENT-FLAG-4113
           MOVE  'N'                   TO ROTENT-START-COUNT-4113
           MOVE  WRK-4128S-CPRODT      TO ROTENT-CPRODT-4113
           MOVE  'DCOM4113'            TO WRK-MODULO.
           MOVE  'S'                   TO WRK-ERRO-ACESSO

           CALL WRK-MODULO  USING ENTRADA-ROTEADOR-4113
                                  SAIDA-ROTEADOR-4113
                                  ERRO-AREA
                                  WRK-SQLCA-416.

           EVALUATE ROTSAI-COD-RETORNO-4113

               WHEN '0000'

                   MOVE 'N'            TO  WRK-ERRO-ACESSO
                   MOVE  1             TO  WRK-IN
                                           WRK-IN2

                   PERFORM 2135-CARREGA-SUBPROD
                           UNTIL   WRK-IN2     GREATER 3 OR
                                   WRK-IN      GREATER 12

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   MOVE '0030'         TO ERR-LOCAL
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE ROTSAI-MSG-RETORNO-4113
                                       TO OUT-MENSAG

           END-EVALUATE.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2135-CARREGA-SUBPROD            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  WRK-LIN-SUBPROD.

           IF   ROTSAI-CSPROD-4113(WRK-IN)   GREATER 0
                MOVE ROTSAI-CSPROD-4113(WRK-IN)
                                              TO  WRK-LIN-SUBP-NRO1
                MOVE ROTSAI-CSGL-SPROD-4113(WRK-IN)
                                              TO  WRK-LIN-SUBP-DESC1
                MOVE '-'   TO TRACO-1
           END-IF

           ADD 1           TO WRK-IN

           IF   ROTSAI-CSPROD-4113(WRK-IN)   GREATER 0
                MOVE ROTSAI-CSPROD-4113(WRK-IN)  TO  WRK-LIN-SUBP-NRO2
                MOVE ROTSAI-CSGL-SPROD-4113(WRK-IN)
                                                 TO  WRK-LIN-SUBP-DESC2
                MOVE ' / ' TO BARRA-1
                MOVE '-'   TO TRACO-2
           END-IF

           ADD 1           TO WRK-IN

           IF WRK-IN NOT LESS 12
              MOVE  13 TO WRK-IN
              GO TO 2135-ACABOU
           END-IF

           IF   ROTSAI-CSPROD-4113(WRK-IN)   GREATER 0
                MOVE ROTSAI-CSPROD-4113(WRK-IN)
                                               TO  WRK-LIN-SUBP-NRO3
                MOVE ROTSAI-CSGL-SPROD-4113(WRK-IN)
                                               TO  WRK-LIN-SUBP-DESC3
                MOVE ' / ' TO BARRA-2
                MOVE '-'   TO TRACO-3
           END-IF

           ADD 1           TO WRK-IN

           IF   ROTSAI-CSPROD-4113(WRK-IN)   GREATER 0
                MOVE ROTSAI-CSPROD-4113(WRK-IN)
                                               TO  WRK-LIN-SUBP-NRO4
                MOVE ROTSAI-CSGL-SPROD-4113(WRK-IN)
                                               TO  WRK-LIN-SUBP-DESC4
                MOVE ' / ' TO BARRA-3
                MOVE '-'   TO TRACO-4
           END-IF.

       2135-ACABOU.

           IF WRK-IN2                EQUAL 1
              MOVE WRK-LIN-SUBPROD      TO  OUT-SPRODT1
           ELSE
            IF WRK-IN2               EQUAL 2
               MOVE WRK-LIN-SUBPROD     TO  OUT-SPRODT2
            ELSE
             IF WRK-IN2              EQUAL 3
                MOVE WRK-LIN-SUBPROD    TO  OUT-SPRODT3
             END-IF
            END-IF
           END-IF.

           ADD   1                      TO  WRK-IN  WRK-IN2.

      *----------------------------------------------------------------*
       2135-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-MENSAGEM          TO  INPUT-DPTM.

           EVALUATE  TRUE
               WHEN  INP-PFK          EQUAL  'H'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK01')
                     PERFORM  2210-DEVOLVER-TELA
                     MOVE INP-MENSAG     TO  OUT-MENSAG

               WHEN  INP-PFK          EQUAL  '3'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK03')
                     PERFORM  2220-RETORNAR-CHAMADOR

               WHEN  INP-PFK          EQUAL  '5'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK05')
                     PERFORM  2230-MENU-ROTINAS

               WHEN  INP-PFK          EQUAL  'A'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK10')
                     PERFORM  2240-MENU-DCOM

               WHEN  INP-PFK          EQUAL  '6'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK06')
                     PERFORM  2250-ACESSAR-DCOM5142
                     PERFORM  2210-DEVOLVER-TELA

               WHEN  INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  SPACES
                     PERFORM  2210-DEVOLVER-TELA
                     MOVE  'TECLE <PF6> PARA EFETIVAR A LIBERACAO OU <PF
      -        '3> PARA RETORNAR'        TO  OUT-MENSAG

               WHEN  OTHER
                     MOVE  'PF INVALIDA'    TO  OUT-MENSAG
                     PERFORM  2210-DEVOLVER-TELA
           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-OPCAO             TO OUT-OPCAO
           MOVE  INP-FASE              TO OUT-FASE
           MOVE  INP-TRANSAC           TO OUT-TRANSAC
           MOVE  INP-TMSTAMP           TO OUT-TMSTAMP
           MOVE  INP-TIPO              TO OUT-TIPO
           MOVE  INP-PAGINA            TO OUT-PAGINA
           MOVE  INP-NOME              TO OUT-NOME
           MOVE  INP-CNPJCPF           TO OUT-CNPJCPF
           MOVE  INP-AGENC             TO OUT-AGENC
           MOVE  INP-CONTA             TO OUT-CONTA
           MOVE  INP-CDPROD            TO OUT-CDPROD
           MOVE  INP-DSCPROD           TO OUT-DSCPROD
           MOVE  INP-CONTRAT           TO OUT-CONTRAT
           MOVE  INP-SPRODT1           TO OUT-SPRODT1
           MOVE  INP-SPRODT2           TO OUT-SPRODT2
           MOVE  INP-SPRODT3           TO OUT-SPRODT3
           MOVE  INP-SITCONT           TO OUT-SITCONT
           MOVE  INP-DATCTR            TO OUT-DATCTR
           MOVE  INP-NRGARNT           TO OUT-NRGARNT
           MOVE  INP-DSGARNT           TO OUT-DSGARNT
           MOVE  INP-DATVCT            TO OUT-DATVCT
           MOVE  INP-PZODIAS           TO OUT-PZODIAS
           MOVE  INP-VLRLIM            TO OUT-VLRLIM
           MOVE  INP-IRENAUT           TO OUT-IRENAUT
           MOVE  INP-ISUBPRD           TO OUT-ISUBPRD
           MOVE  INP-ICTRLIM           TO OUT-ICTRLIM
           MOVE  INP-IISENTO           TO OUT-IISENTO
           MOVE  INP-IVLRFIX           TO OUT-IVLRFIX
           MOVE  INP-IPRCCTR           TO OUT-IPRCCTR
           MOVE  INP-VLTAC             TO OUT-VLTAC.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           IF  INP-TRANSAC                 EQUAL 'DCOM0413'
               INITIALIZE                      COMU-AREA
               MOVE  LENGTH COMU-AREA      TO  COMU-LL
               MOVE  ZEROS                 TO  COMU-ZZ
               MOVE  WRK-CHNG              TO  WRK-FUNCAO
               MOVE  'DCOM0413'            TO  WRK-TELA
               MOVE  'DCOM0416'            TO  COMU-TRANSACAO
               MOVE  INP-SENHAS            TO  COMU-SENHAS
               MOVE  INP-OPCAO             TO  COMU-OPCAO
               MOVE  INP-TMSTAMP           TO  COMU-TIMESTAMP

               IF  WRK-PERFIL              EQUAL 'S'
                   MOVE  INP-CONTRAT-N     TO  COMU-CONTRATO
               END-IF

               IF  WRK-EFETUADO            NOT EQUAL 'S'
                   MOVE  SPACES            TO  COMU-MENSAG
               ELSE
                   MOVE  OUT-MENSAG        TO  COMU-MENSAG
               END-IF
               MOVE  COMU-AREA             TO  WRK-MENSAGEM
           ELSE
            IF INP-TRANSAC                 EQUAL 'DCOM0414'
               INITIALIZE                      COMU-AREA-414
               MOVE  LENGTH COMU-AREA-414  TO  COMU-LL-414
               MOVE  ZEROS                 TO  COMU-ZZ-414
               MOVE  WRK-CHNG              TO  WRK-FUNCAO
               MOVE  'DCOM0414'            TO  WRK-TELA
               MOVE  'DCOM0416'            TO  COMU-TRANSACAO-414
               MOVE  INP-SENHAS            TO  COMU-SENHAS-414
               MOVE  INP-COMANDO           TO  COMU-COMANDO-414
               MOVE  INP-OPCAO             TO  COMU-OPCAO-414
               MOVE  INP-TMSTAMP           TO  COMU-TMSTAMP-414
               MOVE  INP-CONTRAT           TO  COMU-CONTRATO-414
               MOVE  INP-AGENC(01:05)      TO  WRK-AGENCIA
               MOVE  WRK-AGENCIA           TO  COMU-CODAGE-414
               IF  WRK-EFETUADO            NOT EQUAL 'S'
                   MOVE  SPACES            TO  COMU-MENSA-414
               ELSE
                   MOVE  OUT-MENSAG        TO  COMU-MENSA-414
               END-IF
               MOVE  COMU-AREA-414         TO  WRK-MENSAGEM
           ELSE
            IF INP-TRANSAC                 EQUAL 'DCOM0415'
               INITIALIZE                      COMU-AREA-415
               MOVE  LENGTH COMU-AREA-415  TO  COMU-LL-415
               MOVE  ZEROS                 TO  COMU-ZZ-415
               MOVE  WRK-CHNG              TO  WRK-FUNCAO
               MOVE  'DCOM0415'            TO  WRK-TELA
               MOVE  'DCOM0416'            TO  COMU-TRANSACAO-415
               MOVE  INP-SENHAS            TO  COMU-SENHAS-415
               MOVE  INP-COMANDO           TO  COMU-COMANDO-415
               MOVE  INP-OPCAO             TO  COMU-OPCAO-415
               MOVE  INP-TMSTAMP           TO  COMU-TMSTAMP-415
               MOVE  INP-CONTRAT           TO  COMU-CONTRATO-415
               MOVE  INP-PAGINA            TO  COMU-PAGINA-415
               IF  INP-TIPO              EQUAL  1
                   MOVE  INP-AGENC(01:05)    TO COMU-CODAGE-415-X
                   MOVE  INP-CONTA           TO COMU-CONTA-415-X
                   MOVE  SPACES              TO COMU-NUMERO-415-X
                                                COMU-FILIAL-415-X
                                                COMU-CONTROLE-415-X
               ELSE
                   MOVE  SPACES              TO COMU-CODAGE-415-X
                   MOVE  SPACES              TO COMU-CONTA-415-X
                   MOVE  INP-CNPJCPF(01:03)  TO COMU-NUMERO-415-X(01:03)
                   MOVE  INP-CNPJCPF(05:03)  TO COMU-NUMERO-415-X(04:03)
                   MOVE  INP-CNPJCPF(09:03)  TO COMU-NUMERO-415-X(07:03)
                   MOVE  INP-CNPJCPF(13:04)  TO COMU-FILIAL-415-X
                   MOVE  INP-CNPJCPF(18:02)  TO COMU-CONTROLE-415-X
               END-IF
               IF       WRK-EFETUADO            NOT EQUAL 'S'
                        MOVE  SPACES            TO COMU-MENSA-415
               ELSE
                        MOVE  OUT-MENSAG        TO COMU-MENSA-415
               END-IF
               MOVE     COMU-AREA-415               TO  WRK-MENSAGEM
            END-IF
            END-IF
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2230-MENU-ROTINAS               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      COMU-AREA-PFK.
           MOVE  +124                  TO  COMU-LL-PFK.
           MOVE  ZEROS                 TO  COMU-ZZ-PFK.
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  'DCOM0416'            TO  WRK-TELA
                                           COMU-TRANSACAO-PFK.
           MOVE  '5'                   TO  COMU-PFK1.
           MOVE  COMU-AREA-PFK         TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2240-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      COMU-AREA-PFK.

           MOVE  +124                  TO  COMU-LL-PFK.
           MOVE  ZEROS                 TO  COMU-ZZ-PFK.
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  'DCOM0416'            TO  WRK-TELA
                                           COMU-TRANSACAO-PFK.
           MOVE  'A'                   TO  COMU-PFK1.
           MOVE  COMU-AREA-PFK         TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2250-ACESSAR-DCOM5142           SECTION.
      *----------------------------------------------------------------*
      *                    ROTINA DE ATUALIZACAO
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO ROTENT-COD-RETORNO
           MOVE  SPACES                TO ROTENT-MSG-RETORNO
           MOVE  ZEROS                 TO ROTENT-RESTART
           MOVE  SPACES                TO ROTENT-FLAG
           MOVE  LNK-IO-LTERM          TO ROTENT-CTERM
           MOVE  WRK-COD-USER          TO ROTENT-CFUNC-BDSCO
           MOVE  INP-CONTRAT-N         TO ROTENT-CCONTR-LIM-DESC
           MOVE  'DCOM5142'            TO WRK-MODULO.

           CALL WRK-MODULO    USING    ENTRADA-ROTEADOR
                                       SAIDA-ROTEADOR
                                       ERRO-AREA
                                       WRK-SQLCA-416.

           EVALUATE ROTSAI-COD-RETORNO

               WHEN '0000'
                   MOVE 'LIBERACAO EFETUADA COM SUCESSO'
                                               TO OUT-MENSAG
                   MOVE 'S'                    TO WRK-EFETUADO
                   PERFORM 2220-RETORNAR-CHAMADOR

               WHEN '0099'
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE ROTSAI-MSG-RETORNO
                                       TO OUT-MENSAG

           END-EVALUATE.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL  'BRAD0660'            USING  OUTPUT-DPTM
                                              660-DCOMDPTM.

           IF RETURN-CODE              NOT EQUAL  ZEROS
              MOVE  'APL'              TO  ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO  WRK-RETURN-CODE
              MOVE  70                 TO  WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-BRAD0660  TO  ERR-TEXTO
              PERFORM  9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO  WRK-FUNCAO.
           MOVE  OUTPUT-DPTM           TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO    NOT EQUAL  SPACES  AND  LOW-VALUES
              MOVE  'DCOM0416'         TO  ERR-PGM
           ELSE
              MOVE  'SENHAS02'         TO  ERR-PGM
              MOVE  'DCOM0416'         TO  ERR-MODULO
           END-IF.

           MOVE  WRK-COD-USER          TO  ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA-416
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
