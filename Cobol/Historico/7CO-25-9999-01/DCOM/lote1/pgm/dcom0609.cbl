      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM0609.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0609                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: NELSON TANAKA - ALTRAN.                      *
      *     DATA........: 21/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTA CONSULTA OPERACAO - OPERACES P/ CLIENTE*
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMFITM....: LISTA CONSULTA OPERACAO - OPERACOES P/ CONV. *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     DCOM5520 - OBTER OPERACOES POR AGENCIA                     *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0103                                     *
      *================================================================*
DPJ22 *                    ALTERACAO                                   *
DPJ22 *----------------------------------------------------------------*
DPJ22 *    PROGRAMADOR.:   LEANDRO TAKAOKA           - BRADESCO        *
DPJ22 *    DATA........:   16/09/2025                                  *
DPJ22 *----------------------------------------------------------------*
DPJ22 *    OBJETIVO....: LIBERAR ACESSO A TERCEIROS  DLVRPJ-22         *
      *
      *
      *================================================================*
       ENVIRONMENT                         DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                       SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT                   IS COMMA.
      *
      *================================================================*
       DATA                                DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** DCOM0609 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  IND-1                           PIC S9(09) COMP VALUE ZEROS.
       77  WRK-IND                         PIC S9(09) COMP VALUE ZEROS.
       77  WIND-SEL                        PIC S9(09) COMP VALUE ZEROS.
       77  WCT-SELEC                       PIC S9(09) COMP VALUE ZEROS.
       77  WRK-VOPER-E                     PIC ZZZ.ZZZ.ZZ9,99.
      *
      *---------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-225                         PIC S9(08) COMP VALUE +225.
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-241                         PIC S9(08) COMP VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       01  WRK-49361                       PIC S9(08) COMP VALUE +49361.
      *--- ALPHA    - NORMAL    - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49345                       PIC S9(08) COMP VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                       PIC S9(08) COMP VALUE +49369.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(008)     VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)     VALUE SPACES.
           05  WRK-CONTA-CONS          PIC  9(13) COMP-3.
           05  WRK-CONTA-CONS-R        PIC  9(13)          VALUE ZEROS.
           05  WRK-DIGITO              PIC  X(01)          VALUE SPACES.
           05  WRK-DIGITO-R REDEFINES WRK-DIGITO PIC 9(01).
           05  WRK-TAMANHO             PIC  9(02)          VALUE 7.

       01  WRK-AGENCIA-AUX.
           05  WRK-AGENCIA-NUM         PIC  9(005)         VALUE ZEROS.

       01  WRK-LIT-CNPJ-TELA.
FS2511*    05  WRK-CNPJ-TELA           PIC  9(009)     VALUE ZEROS.
FS2511     05  WRK-CNPJ-TELA           PIC  X(09)      VALUE SPACES.
           05  FILLER                  PIC  X(001)     VALUE '/'.
FS2511*    05  WRK-FILIAL-TELA         PIC  9(004)     VALUE ZEROS.
FS2511     05  WRK-FILIAL-TELA         PIC  X(004)     VALUE SPACES.
           05  FILLER                  PIC  X(001)     VALUE '-'.
           05  WRK-CTRL-TELA           PIC  9(002)     VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO DCOM7995 **'.
      *----------------------------------------------------------------*

       01  WRK-ENTRADA-DCOM7995.
           05  WRK-ENT-OPCAO           PIC  X(02)          VALUE SPACES.
           05  WRK-ENT-COD-FUNC        PIC  9(09)          VALUE ZEROS.
           05  WRK-ENT-COD-TERMINAL    PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-TIMESTAMP       PIC  X(26)          VALUE SPACES.
           05  WRK-ENT-TRANS           PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-TRANS-ANT       PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-PAGINA          PIC  9(03)          VALUE ZEROS.
           05  WRK-ENT-DADOS           PIC  X(1600)        VALUE SPACES.

       01  WRK-SAIDA-DCOM7995.
           05  WRK-SAI-COD-RETORNO     PIC  9(02)          VALUE ZEROS.
           05  WRK-SAI-MSG-RETORNO     PIC  X(79)          VALUE SPACES.
           05  WRK-SAI-TIMESTAMP       PIC  X(26)          VALUE SPACES.
           05  WRK-SAI-TRANS           PIC  X(08)          VALUE SPACES.
           05  WRK-SAI-TRANS-ANT       PIC  X(08)          VALUE SPACES.
           05  WRK-SAI-PAGINA          PIC  9(03)          VALUE ZEROS.
           05  WRK-SAI-DADOS           PIC  X(1600)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*
      *
      *   NOTA:
      *
      *   A NAVEGACAO DE CONSULTA DE OPERACAO EH MUITO GRANDE, PORTANTO
      * A AREA DE COMUNICACAO SERA SALVA NO DBTEMPORARIO, JUNTAMENTE
      * COM OS CHAMADORES E SUAS PAGINAS, UTILIZAR ESSA AREA COMO PADRAO
      * E QUALQUER CAMPO QUE SEJA ADICIONADO NESSA AREA DEVERA SER COLO-
      * CADO APOS AO ULTIMO CAMPO.
      *----------------------------------------------------------------*

       COPY 'I#DCOMJ2'.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM0603 ***'.
      *----------------------------------------------------------------*
       01          COMU-AREA-IMS.
         05        COMU-LL                 PIC S9(04) COMP VALUE +406.
         05        COMU-ZZ                 PIC S9(04) COMP VALUE ZEROS.
         05        COMU-TRANCODE.
            10     COMU-TRANSACAO          PIC X(008)      VALUE SPACES.
            10     FILLER                  PIC X(006)      VALUE SPACES.
            10     COMU-PFK                PIC X(001)      VALUE SPACES.
         05        COMU-SENHAS             PIC X(037)      VALUE SPACES.
         05        COMU-COMANDO            PIC X(068)      VALUE SPACES.
         05        COMU-DADOS.
            10     COMU-TIMESTAMP          PIC X(026)      VALUE SPACES.
            10     COMU-MENSAGEM           PIC X(079)      VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-PFK.
           05  COMU-LL-PFK                 PIC S9(04) COMP VALUE +124.
           05  COMU-ZZ-PFK                 PIC S9(04) COMP VALUE ZEROS.
           05  COMU-TRANCODE-PFK.
               10  COMU-TRANSACAO-PFK      PIC  X(08)      VALUE SPACES.
               10  FILLER                  PIC  X(06)      VALUE SPACES.
               10  COMU-PFK-PFK            PIC  X(01)      VALUE SPACES.
           05  COMU-SENHAS-PFK             PIC  X(37)      VALUE SPACES.
           05  COMU-COMANDO-PFK            PIC  X(68)      VALUE SPACES.
      *
      *---------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-SQLCA                   PIC  X(136)     VALUE SPACES.
           05  WRK-GU                      PIC  X(04)      VALUE 'GU  '.
           05  WRK-CHNG                    PIC  X(04)      VALUE 'CHNG'.
           05  WRK-ISRT                    PIC  X(04)      VALUE 'ISRT'.
           05  WRK-FUNCAO                  PIC  X(04)      VALUE SPACES.
           05  WRK-TELA                    PIC  X(08)      VALUE SPACES.
           05  WRK-VERSAO                  PIC  X(06)      VALUE
                                                           'VRS002'.
           05  WRK-COD-USER                PIC  X(07)      VALUE SPACES.
           05  WRK-COD-USER-R              REDEFINES
               WRK-COD-USER                PIC  9(07).
           05  WRK-COD-DEPTO.
               10  FILLER                  PIC  X(02)      VALUE SPACES.
               10  WRK-COD-DEPTO-N         PIC  9(04)      VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER                  PIC  X(04)      VALUE SPACES.
               10  WRK-TRANSACAO           PIC  X(08)      VALUE SPACES.
               10  FILLER                  PIC  X(1988)    VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFITM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMFITM.
           05  WRK-660-LL-AREA-FITM        PIC  9(04) COMP VALUE 0188.
           05  WRK-660-LL-MENS-FITM        PIC  9(04) COMP VALUE 1022.
           05  WRK-660-SENHAS-FITM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FITM        PIC  9(04) COMP VALUE 2070.
           05  WRK-660-TMSTAMP-FITM        PIC  9(04) COMP VALUE 0026.
           05  WRK-660-ULTPAG-FITM         PIC  9(04) COMP VALUE 0001.
           05  WRK-660-PAGINA-FITM         PIC  9(04) COMP VALUE 0004.
           05  WRK-660-CONVEN-FITM         PIC  9(04) COMP VALUE 0009.
           05  WRK-660-NMCONV-FITM         PIC  9(04) COMP VALUE 0015.
           05  WRK-660-AGENC-FITM          PIC  9(04) COMP VALUE 0005.
           05  WRK-660-NMAGEN-FITM         PIC  9(04) COMP VALUE 0040.
           05  WRK-660-NSITU-FITM          PIC  9(04) COMP VALUE 0060.
           05  WRK-600-LISTA               OCCURS  10.
               10 WRK-660-SEL-FITM         PIC  9(04) COMP VALUE 2003.
               10 WRK-660-OPER-FITM        PIC  9(04) COMP VALUE 0013.
               10 WRK-660-PROD-FITM        PIC  9(04) COMP VALUE 0006.
               10 WRK-660-SUBP-FITM        PIC  9(04) COMP VALUE 0006.
               10 WRK-660-NOME-FITM        PIC  9(04) COMP VALUE 0010.
               10 WRK-660-DINC-FITM        PIC  9(04) COMP VALUE 0010.
               10 WRK-660-VOPE-FITM        PIC  9(04) COMP VALUE 0014.
               10 WRK-660-SITU-FITM        PIC  9(04) COMP VALUE 0006.
               10 WRK-660-AGE-FITM         PIC  9(04) COMP VALUE 0005.
           05  WRK-660-MENSA-FITM          PIC  9(04) COMP VALUE 0079.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.
           05  FILLER                      PIC  X(45)      VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                      PIC  X(12)      VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE             PIC  9(02)      VALUE ZEROS.
           05  FILLER                      PIC  X(11)      VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO              PIC  9(02)      VALUE ZEROS.
           05  FILLER                      PIC  X(02)      VALUE ' *'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOMFITM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-FITM.
           05  FILLER                      PIC  X(18)      VALUE SPACES.
           05  WRK-INP-PFK-FITM            PIC  X(01)      VALUE SPACES.
           05  WRK-INP-SENHAS-FITM         PIC  X(37)      VALUE SPACES.
           05  WRK-INP-COMANDO-FITM        PIC  X(68)      VALUE SPACES.
           05  WRK-INP-DADOS-FITM.
               10  WRK-INP-TMSTAMP-FITM    PIC  X(26)      VALUE SPACES.
               10  WRK-INP-ULTPAG-FITM     PIC  X(01)      VALUE SPACE.
               10  WRK-INP-PAGINA-FITM-X.
                   15  WRK-INP-PAGINA-FITM
                                           PIC  9(04)      VALUE ZEROS.
      *
               10  WRK-INP-CONVEN-FITM     PIC  X(09)      VALUE SPACES.
               10  WRK-INP-NMCONV-FITM     PIC  X(15)      VALUE SPACES.
               10  WRK-INP-AGENC-FITM      PIC  X(05)      VALUE SPACES.
               10  WRK-INP-NMAGEN-FITM     PIC  X(40)      VALUE SPACES.
               10  WRK-INP-NSITU-FITM      PIC  X(60)      VALUE SPACES.
      *
               10  WRK-INP-LINHAS-FITM OCCURS 10 TIMES.
                   15  WRK-INP-SELEC-FITM
                                           PIC  X(01)      VALUE SPACES.
                   15  WRK-INP-OPER-FITM-X.
                       20  WRK-INP-OPER-FITM
                                           PIC  9(13)      VALUE ZEROS.
                   15  WRK-INP-PROD-FITM   PIC  X(06)      VALUE SPACES.
                   15  WRK-INP-SUBP-FITM   PIC  X(06)      VALUE SPACES.
                   15  WRK-INP-NOME-FITM   PIC  X(10)      VALUE SPACES.
                   15  WRK-INP-DINC-FITM   PIC  X(10)      VALUE SPACES.
                   15  WRK-INP-VOPE-FITM   PIC  X(14)      VALUE SPACES.
                   15  WRK-INP-SITU-FITM   PIC  X(06)      VALUE SPACES.
                   15  WRK-INP-AGE-FITM    PIC  X(05)      VALUE SPACES.
           05  WRK-INP-MENSA-FITM          PIC  X(79)      VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFITM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-FITM.
           05  WRK-OUT-LL-FITM             PIC S9(04) COMP VALUE +1022.
           05  WRK-OUT-ZZ-FITM             PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FITM         PIC  X(37)      VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-FITM   PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-COMANDO-FITM        PIC  X(68)      VALUE SPACES.
           05  WRK-OUT-DADOS-FITM.
               10  WRK-OUT-TMSTAMP-FITM    PIC  X(26)      VALUE SPACES.
               10  WRK-OUT-ULTPAG-FITM     PIC  X(01)      VALUE SPACE.
               10  WRK-OUT-PAGINA-FITM-X.
                   15  WRK-OUT-PAGINA-FITM PIC  9(04)      VALUE ZEROS.
      *
               10  WRK-OUT-CONVEN-FITM     PIC  X(09)      VALUE SPACES.
               10  WRK-OUT-NMCONV-FITM     PIC  X(15)      VALUE SPACES.
               10  WRK-OUT-AGENC-FITM      PIC  X(05)      VALUE SPACES.
               10  WRK-OUT-NMAGEN-FITM     PIC  X(40)      VALUE SPACES.
               10  WRK-OUT-NSITU-FITM      PIC  X(60)      VALUE SPACES.
      *
               10  WRK-OUT-LINHAS-FITM OCCURS 10 TIMES.
                   15  WRK-OUT-SELEC-ATTR-FITM
                                           PIC S9(04) COMP VALUE ZEROS.
                   15  WRK-OUT-SELEC-FITM  PIC  X(01)      VALUE SPACES.
                   15  WRK-OUT-OPER-FITM-X.
                       20  WRK-OUT-OPER-FITM
                                           PIC  9(13)      VALUE ZEROS.
                   15  WRK-OUT-PROD-FITM   PIC  X(06)      VALUE SPACES.
                   15  WRK-OUT-SUBP-FITM   PIC  X(06)      VALUE SPACES.
                   15  WRK-OUT-NOME-FITM   PIC  X(10)      VALUE SPACES.
                   15  WRK-OUT-DINC-FITM   PIC  X(10)      VALUE SPACES.
                   15  WRK-OUT-VOPE-FITM   PIC  X(14)      VALUE SPACES.
                   15  WRK-OUT-SITU-FITM   PIC  X(06)      VALUE SPACES.
                   15  WRK-OUT-AGE-FITM    PIC  X(05)      VALUE SPACES.
           05  WRK-OUT-MENSA-FITM          PIC  X(79)      VALUE SPACES.

      *----------------------------------------------------------------*
      * I#DCOMX3                                                       *
      * AREA DE COMUNICACAO COM O  MODULO  DCOMXXXX P/ SELECAO DE      *
      *         CONTAS E  PESSOAS CADASTRADAS NO CADU
      *----------------------------------------------------------------*
       COPY 'I#DCOMX3'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DO MODULO DCOM5520 ***'.
      *----------------------------------------------------------------*
      *--- AREA DE ENTRADA DO ROTEADOR --> TAM. 0114 BYTES          ---*
      *
       01  ENTRADA-ROT5520.
           05  ENT5520-HEADER.
               10  ENT5520-COD-RETORNO               PIC  X(004).
               10  ENT5520-MSG-RETORNO               PIC  X(079).
               10  ENT5520-RESTART                   PIC  9(005).
               10  ENT5520-FLAG                      PIC  9(001).
               10  ENT5520-START-COUNT               PIC  X(001).
               10  ENT5520-CFUNC-BDSCO               PIC  9(009).
               10  ENT5520-CTERM                     PIC  X(008).
           05  ENT5520-DADOS.
               10  ENT5520-FLAG-FILTRO             PIC  X(002).
               10  ENT5520-CBCO                    PIC  9(003).
               10  ENT5520-CAG-BCRIA               PIC  9(005).
               10  ENT5520-CAG-OPER-OPER           PIC  9(005).
               10  ENT5520-CCTA-BCRIA-CLI          PIC  9(013).
               10  ENT5520-CCONVE-CLI-DESC         PIC  9(009).
               10  ENT5520-CCONVE-GRAL-DESC        PIC  9(009).
               10  ENT5520-CCONTR-LIM-DESC         PIC  9(009).
               10  ENT5520-CPRODT                  PIC  9(003).
               10  ENT5520-CSPROD-DESC-COML        PIC  9(003).
               10  ENT5520-DINIC-OPER-DESC         PIC  X(010).
               10  ENT5520-DFNAL-OPER-DESC         PIC  X(010).
               10  ENT5520-CELMTO-DESC-COML        PIC  9(003).
FS2511*        10  ENT5520-CCNPJ-CPF               PIC  9(009).
FS2511         10  ENT5520-CCNPJ-CPF               PIC  X(009).
FS2511*        10  ENT5520-CFLIAL-CNPJ             PIC  9(005).
FS2511         10  ENT5520-CFLIAL-CNPJ             PIC  X(004).
               10  ENT5520-CCTRL-CNPJ-CPF          PIC  9(002).
FS2511*        10  ENT5520-CCNPJ-CPF-SAC           PIC  9(009).
FS2511         10  ENT5520-CCNPJ-CPF-SAC           PIC  X(009).
FS2511*        10  ENT5520-CFLIAL-CNPJ-SAC         PIC  9(005).
FS2511         10  ENT5520-CFLIAL-CNPJ-SAC         PIC  X(004).
               10  ENT5520-CCTRL-CNPJ-CPF-SAC      PIC  9(002).
               10  ENT5520-CSIT-DESC-COML          PIC  9(003).
               10  ENT5520-CINDCD-EVNTO-WORKFLOW   PIC  9(003).
               10  ENT5520-CINDCD-SIT-FINAL-EVNTO  PIC  9(003).
               10  ENT5520-COD-SITUACAO            PIC  9(003).
               10  ENT5520-CTPO-BAIXA-PCELA        PIC  X(001).
               10  ENT5520-DEFETV-BAIXA-INI        PIC  X(010).
               10  ENT5520-DEFETV-BAIXA-FIM        PIC  X(010).
      *
      *--- AREA DE SAIDA   DO ROTEADOR --> TAM. 2163 BYTES          ---*
      *
       01  SAIDA-ROT5520.
           05  SAI5520-HEADER.
               10  SAI5520-COD-RETORNO               PIC  X(004).
               10  SAI5520-MSG-RETORNO               PIC  X(079).
               10  SAI5520-RESTART                   PIC  9(005).
               10  SAI5520-FLAG                      PIC  X(001).
               10  SAI5520-QTDE-RETORNADA            PIC  9(003).
               10  SAI5520-COUNT                     PIC  9(009).
      *
           05  SAI5520-DADOS.
               10  SAI5520-TABELA-SAIDA     OCCURS 10  TIMES.
                   15 SAI5520-DANO-OPER-DESC        PIC  9(004).
                   15 SAI5520-NSEQ-OPER-DESC        PIC  9(009).
                   15 SAI5520-IPSSOA-DESC-COML      PIC  X(060).
                   15 SAI5520-CBCO                  PIC  9(003).
                   15 SAI5520-CAG-BCRIA             PIC  9(005).
                   15 SAI5520-CAG-BCRIA-DESC        PIC  X(040).
                   15 SAI5520-CCTA-BCRIA-CLI        PIC  9(013).
                   15 SAI5520-CBCO-OPER-OPER        PIC  9(003).
                   15 SAI5520-CAG-OPER-OPER         PIC  9(005).
                   15 SAI5520-CSIT-DESC-COML        PIC  9(003).
                   15 SAI5520-IRSUMO-SIT-DESC       PIC  X(015).
                   15 SAI5520-CSGL-SIT-DESC         PIC  X(006).
                   15 SAI5520-CPRODT                PIC  9(003).
                   15 SAI5520-IABREV-PRODT          PIC  X(012).
                   15 SAI5520-CSGL-PRODT-DESC       PIC  X(006).
                   15 SAI5520-CSPROD-DESC-COML      PIC  9(003).
                   15 SAI5520-CTPO-DESC-COML        PIC  9(003).
                   15 SAI5520-IRSUMO-SPROD-DESC     PIC  X(010).
                   15 SAI5520-CSGL-SPROD-DESC       PIC  X(006).
                   15 SAI5520-VOPER-DESC-COML       PIC  9(015)V9(2).
                   15 SAI5520-DINIC-OPER-DESC       PIC  X(010).
                   15 SAI5520-DVCTO-FNAL-OPER       PIC  X(010).
                   15 SAI5520-CMEIO-ENTRD-DESC      PIC  9(003).
                   15 SAI5520-IMEIO-ENTRD-DESC      PIC  X(025).
                   15 SAI5520-IRSUMO-MEIO-ENTRD     PIC  X(015).
                   15 SAI5520-CSGL-MEIO-ENTRD       PIC  X(006).
                   15 SAI5520-CINDCD-LIBRC-ANTCP    PIC  X(001).
                   15 SAI5520-PARECER-FORML         PIC  X(001).
                   15 SAI5520-VLIQ-OPER-DESC        PIC  9(015)V9(2).
      *
           05  SAI5520-CONSISTENCIA REDEFINES SAI5520-DADOS.
               10  SAI5520-TAMANHO-ERRO     PIC  9(001).
               10  SAI5520-TABELA-ERROS     OCCURS  2 TIMES.
                   15  SAI5520-CAMPOS-ERRO  PIC  9(001).
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMSY'.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** DCOM0609 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       LINKAGE                             SECTION.
      *----------------------------------------------------------------*
      *
       01  LNK-IO-PCB.
           05 LNK-IO-LTERM                 PIC  X(08).
           05 FILLER                       PIC  X(02).
           05 LNK-IO-STATUS                PIC  X(02).
           05 FILLER                       PIC  X(12).
           05 LNK-IO-MODNAME               PIC  X(08).
      *
       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM                PIC  X(08).
           05 FILLER                       PIC  X(02).
           05 LNK-ALT-STATUS               PIC  X(02).
           05 FILLER                       PIC  X(12).
           05 LNK-ALT-MODNAME              PIC  X(08).
      *
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING  LNK-IO-PCB
                                              LNK-ALT-PCB.

           MOVE WRK-GU                     TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       0000-INICIAR                        SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING  WRK-VERSAO
                                              WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              WRK-MENSAGEM
                                              WRK-TELA
                                              WRK-COD-USER
                                              WRK-COD-DEPTO.

           IF  RETURN-CODE                 EQUAL 04 OR 12
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                     TO WRK-OUT-DADOS-FITM
                                              WRK-OUT-SENHAS-FITM
                                              WRK-OUT-COMANDO-FITM
                                              COMU-DADOS.

           INITIALIZE                         ENTRADA-ROT5520
                                              SAIDA-ROT5520

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     PROCESSAR MENSAGEM                                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM             SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMFITM'                 TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0609'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFITM
           END-IF.

           IF  WRK-TELA                    EQUAL 'DCOMFITM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RECEBER CONTROLE                                           *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO COMU-AREA-IMS
           MOVE COMU-TIMESTAMP             TO WRK-OUT-TMSTAMP-FITM
                                              WRK-INP-TMSTAMP-FITM
                                              WRK-ENT-TIMESTAMP.

           MOVE 1                          TO WRK-ENT-PAGINA
                                              WRK-OUT-PAGINA-FITM.
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL


           IF  WRK-CONVENIO-CLIENTE       GREATER ZEROS
               MOVE WRK-CONVENIO-CLIENTE  TO WRK-INP-CONVEN-FITM
                                             WRK-OUT-CONVEN-FITM
               MOVE 'CONV. CLIENTE'       TO WRK-INP-NMCONV-FITM
                                             WRK-OUT-NMCONV-FITM
           ELSE
               MOVE WRK-CONVENIO-GERAL    TO WRK-INP-CONVEN-FITM
                                             WRK-OUT-CONVEN-FITM
               MOVE 'CONV. GERAL  '       TO WRK-INP-NMCONV-FITM
                                             WRK-OUT-NMCONV-FITM
           END-IF.

           IF  WRK-AGENCIA-LISTA           EQUAL ZEROS
               MOVE SPACES                 TO WRK-OUT-AGENC-FITM
                                              WRK-OUT-NMAGEN-FITM
           ELSE
               MOVE WRK-AGENCIA-LISTA      TO WRK-OUT-AGENC-FITM
           END-IF.

           IF  WRK-SITUACAO-LISTA          EQUAL ZEROS
               MOVE 'TODOS'                TO WRK-OUT-NSITU-FITM
           ELSE
               EVALUATE WRK-SITUACAO-LISTA
                   WHEN 1
                      MOVE 'TODOS'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 2
                      MOVE 'EM CADASTRAMENTO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 3
                      MOVE 'CADASTRADO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 4
                      MOVE 'ATIVO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 5
                      MOVE 'FORMALIZADO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 6
                      MOVE 'VENCIDO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 7
                      MOVE 'VENCIDO EM C.A.'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 8
                      MOVE 'ENCERRADO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 9
                      MOVE 'ADITADO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 10
                      MOVE 'ENCERRADO POR ADITAMENTO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 11
                      MOVE 'ENCERRADO POR RENOVACAO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 12
                      MOVE 'ENCERRADO POR BAIXA'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 13
                      MOVE 'ENCERRADO POR TRANSFERENCIA DE LP'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 14
                      MOVE 'REATIVADO OU REABERTO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 15
                      MOVE 'ESTORNADO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 16
                      MOVE 'CANCELADO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 17
                      MOVE 'BLOQUEADO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 18
                      MOVE 'AGUARDANDO VIGENCIA'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 19
                      MOVE 'PAGTO CHEQUE VINCULADO VENCIDO NORMAL'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 20
                      MOVE 'PAGTO CHEQUE VINCULADO VENCIDO C.A.'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 21
                      MOVE 'PENDENTE'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 22
                      MOVE 'CONCLUIDO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 23
                      MOVE 'AGUARDANDO ATIVACAO'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 24
                      MOVE 'A VENCER'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 25
                      MOVE 'BAIXA VALIDADA'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 26
                      MOVE 'BAIXA COMANDADA DE PARCELA'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 37
                      MOVE 'PAGTO CHEQUE VINCULADO A VENCER'
                                           TO WRK-OUT-NSITU-FITM
                   WHEN 28
                      MOVE 'EM RENOVACAO'
                                           TO WRK-OUT-NSITU-FITM
               END-EVALUATE
           END-IF

           INITIALIZE  ENTRADA-ROT5520
                       SAIDA-ROT5520

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15

               IF  WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0609'
                   COMPUTE ENT5520-RESTART = (WRK-PAGINA(IND-1) - 1)
                                             * 10
                   MOVE WRK-PAGINA(IND-1)    TO WRK-OUT-PAGINA-FITM
               END-IF
           END-PERFORM.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0612'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FITM       TO WRK-ENT-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           PERFORM 7000-ACESSAR-DCOM5520.

      *----------------------------------------------------------------*
       2100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8200-ACESSAR-DCOM7995           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE 'DCOM0601'             TO WRK-ENT-TRANS

           MOVE 'DCOM7995'             TO WRK-MODULO.

           CALL WRK-MODULO          USING WRK-ENTRADA-DCOM7995
                                          WRK-SAIDA-DCOM7995
                                          ERRO-AREA
                                          WRK-SQLCA.

           IF  WRK-SAI-COD-RETORNO     EQUAL 01
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-SAI-MSG-RETORNO
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-SAI-COD-RETORNO EQUAL 99
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       8200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2110-MONTAR-TELA                    SECTION.
      *----------------------------------------------------------------*

           IF (SAI5520-DANO-OPER-DESC(WRK-IND)   NOT NUMERIC) OR
              (SAI5520-DANO-OPER-DESC(WRK-IND)   EQUAL ZEROS)
               MOVE WRK-225                TO
                   WRK-OUT-SELEC-ATTR-FITM(WRK-IND)
           ELSE
               MOVE SAI5520-DANO-OPER-DESC(WRK-IND)
                                           TO
                    WRK-OUT-OPER-FITM(WRK-IND)(01:04)
               MOVE SAI5520-NSEQ-OPER-DESC(WRK-IND)
                                           TO
                    WRK-OUT-OPER-FITM(WRK-IND)(05:09)
               MOVE SAI5520-CSGL-PRODT-DESC(WRK-IND)
                                           TO WRK-OUT-PROD-FITM(WRK-IND)
               MOVE SAI5520-CSGL-SPROD-DESC(WRK-IND)
                                           TO WRK-OUT-SUBP-FITM(WRK-IND)
               MOVE SAI5520-IPSSOA-DESC-COML(WRK-IND)
                                           TO WRK-OUT-NOME-FITM(WRK-IND)
               MOVE SAI5520-CAG-OPER-OPER(WRK-IND)
                                           TO WRK-OUT-AGE-FITM(WRK-IND)
               MOVE SAI5520-DINIC-OPER-DESC(WRK-IND)
                                           TO WRK-OUT-DINC-FITM(WRK-IND)
               INSPECT WRK-OUT-DINC-FITM(WRK-IND)
                                           REPLACING ALL '.' BY '/'
               MOVE SAI5520-VOPER-DESC-COML(WRK-IND)
                                           TO WRK-VOPER-E
               MOVE WRK-VOPER-E            TO WRK-OUT-VOPE-FITM(WRK-IND)
               MOVE SAI5520-IRSUMO-SIT-DESC(WRK-IND)
                                           TO WRK-OUT-SITU-FITM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2115-LIMPAR-TELA                    SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-225                    TO
               WRK-OUT-SELEC-ATTR-FITM(WRK-IND)
           MOVE SPACES                     TO
                WRK-OUT-OPER-FITM-X(WRK-IND)

           MOVE SPACES                   TO WRK-OUT-PROD-FITM(WRK-IND)
                                            WRK-OUT-SUBP-FITM(WRK-IND)
                                            WRK-OUT-NOME-FITM(WRK-IND)
                                            WRK-OUT-DINC-FITM(WRK-IND)
                                            WRK-OUT-VOPE-FITM(WRK-IND)
                                            WRK-OUT-SITU-FITM(WRK-IND)
                                            WRK-OUT-AGE-FITM (WRK-IND).

      *----------------------------------------------------------------*
       2115-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOMFITM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFITM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FITM

           PERFORM 2205-DEVOLVER-TELA-FIXO

           EVALUATE TRUE
             WHEN WRK-INP-PFK-FITM         EQUAL 'H' OR
                  (WRK-INP-PFK-FITM        EQUAL '.' AND
                   WRK-INP-COMANDO-FITM    EQUAL 'PFK01')
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   IF  WRK-INP-OPER-FITM-X(1)  NOT NUMERIC OR
                       WRK-INP-OPER-FITM-X(1)  EQUAL SPACES
                       MOVE WRK-49345      TO WRK-OUT-COMANDO-ATTR-FITM
                   END-IF
                   MOVE WRK-INP-MENSA-FITM TO  WRK-OUT-MENSA-FITM

             WHEN WRK-INP-PFK-FITM         EQUAL '3' OR
                   (WRK-INP-PFK-FITM       EQUAL '.' AND
                    WRK-INP-COMANDO-FITM   EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FITM       EQUAL 'X' OR
                   (WRK-INP-PFK-FITM       EQUAL '.' AND
                    WRK-INP-COMANDO-FITM   EQUAL 'PFK05')
                  PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-FITM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FITM       EQUAL '.' AND
                    WRK-INP-COMANDO-FITM   EQUAL 'PFK10')
                  PERFORM 2240-MENU-DCOM

             WHEN WRK-INP-PFK-FITM         EQUAL '7' OR
                 (WRK-INP-PFK-FITM         EQUAL '.' AND
                  WRK-INP-COMANDO-FITM     EQUAL 'PFK07')
                  PERFORM 2250-TELA-ANTERIOR

             WHEN WRK-INP-PFK-FITM         EQUAL '8' OR
                 (WRK-INP-PFK-FITM         EQUAL '.' AND
                  WRK-INP-COMANDO-FITM     EQUAL 'PFK08')
                  PERFORM 2260-TELA-POSTERIOR

             WHEN (WRK-INP-PFK-FITM        EQUAL '.' AND
                   WRK-INP-COMANDO-FITM    EQUAL SPACES)
                   PERFORM 2270-TRATAR-ENTER
                   PERFORM 2210-DEVOLVER-TELA-VARIA

             WHEN  OTHER
                   IF  WRK-INP-OPER-FITM-X(1)  NOT NUMERIC OR
                       WRK-INP-OPER-FITM-X(1)  EQUAL SPACES
                       MOVE WRK-49345      TO WRK-OUT-COMANDO-ATTR-FITM
                   END-IF
                   MOVE SG-MENSAGEM(38)    TO  WRK-OUT-MENSA-FITM
                   PERFORM 2210-DEVOLVER-TELA-VARIA

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS FIXOS                                *
      *----------------------------------------------------------------*
       2205-DEVOLVER-TELA-FIXO             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TMSTAMP-FITM       TO WRK-OUT-TMSTAMP-FITM
           MOVE WRK-INP-ULTPAG-FITM        TO WRK-OUT-ULTPAG-FITM
           MOVE WRK-INP-PAGINA-FITM-X      TO WRK-OUT-PAGINA-FITM-X
           MOVE WRK-INP-CONVEN-FITM        TO WRK-OUT-CONVEN-FITM
           MOVE WRK-INP-NMCONV-FITM        TO WRK-OUT-NMCONV-FITM
           MOVE WRK-INP-AGENC-FITM         TO WRK-OUT-AGENC-FITM
           MOVE WRK-INP-NMAGEN-FITM        TO WRK-OUT-NMAGEN-FITM
           MOVE WRK-INP-NSITU-FITM         TO WRK-OUT-NSITU-FITM.
           MOVE WRK-INP-MENSA-FITM         TO WRK-OUT-MENSA-FITM.

      *----------------------------------------------------------------*
       2205-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS VARIAVEIS                            *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA-VARIA            SECTION.
      *----------------------------------------------------------------*

           PERFORM  VARYING  WRK-IND  FROM 1 BY 1
                      UNTIL  WRK-IND  GREATER 10

               IF (WRK-INP-OPER-FITM(WRK-IND) NOT NUMERIC) OR
                  (WRK-INP-OPER-FITM(WRK-IND) EQUAL ZEROS)
                   MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-FITM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-FITM(WRK-IND)
                                     TO WRK-OUT-SELEC-FITM(WRK-IND)
                   MOVE WRK-INP-OPER-FITM-X(WRK-IND)
                                     TO WRK-OUT-OPER-FITM-X(WRK-IND)
                   MOVE WRK-INP-PROD-FITM(WRK-IND)
                                     TO WRK-OUT-PROD-FITM(WRK-IND)
                   MOVE WRK-INP-SUBP-FITM(WRK-IND)
                                     TO WRK-OUT-SUBP-FITM(WRK-IND)
                   MOVE WRK-INP-NOME-FITM(WRK-IND)
                                     TO WRK-OUT-NOME-FITM(WRK-IND)
                   MOVE WRK-INP-DINC-FITM(WRK-IND)
                                     TO WRK-OUT-DINC-FITM(WRK-IND)
                   MOVE WRK-INP-VOPE-FITM(WRK-IND)
                                     TO WRK-OUT-VOPE-FITM(WRK-IND)
                   MOVE WRK-INP-SITU-FITM(WRK-IND)
                                     TO WRK-OUT-SITU-FITM(WRK-IND)
                   MOVE WRK-INP-AGE-FITM (WRK-IND)
                                     TO WRK-OUT-AGE-FITM (WRK-IND)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM0103                   *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-IMS
           MOVE SPACES                     TO COMU-DADOS

           MOVE WRK-INP-TMSTAMP-FITM       TO WRK-ENT-TIMESTAMP
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               MOVE WRK-CHAMADOR(IND-1)    TO WRK-TELA
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FITM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE SPACES                     TO WRK-CHAMADOR(IND-1 - 1).
           MOVE ZEROS                      TO WRK-PAGINA(IND-1 - 1)
                                              WRK-CONTRATO
                                              WRK-CONVENIO-CLIENTE
                                              WRK-CONVENIO-GERAL.
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE LENGTH COMU-AREA-IMS       TO COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE WRK-OUT-MENSA-FITM         TO COMU-MENSAGEM
           MOVE 'DCOM0609'                 TO COMU-TRANSACAO
           MOVE '3'                        TO COMU-PFK
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
       2230-MENU-ROTINAS                   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TMSTAMP-FITM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0609'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE '5'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2230-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO MENU DCOM                                      *
      *----------------------------------------------------------------*
       2240-MENU-DCOM                      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TMSTAMP-FITM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0614'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE 'A'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR TELA ANTERIOR                                     *
      *----------------------------------------------------------------*
       2250-TELA-ANTERIOR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-FITM         EQUAL 1
               IF  WRK-INP-OPER-FITM-X(1)  NOT NUMERIC OR
                   WRK-INP-OPER-FITM-X(1)  EQUAL SPACES
                   MOVE WRK-49345          TO WRK-OUT-COMANDO-ATTR-FITM
               END-IF
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(28)        TO WRK-OUT-MENSA-FITM
               GO TO 2250-99-FIM
           END-IF.

           MOVE SPACES                    TO WRK-INP-ULTPAG-FITM
                                             WRK-OUT-ULTPAG-FITM.

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      ENTRADA-ROT5520.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FITM       TO WRK-ENT-TIMESTAMP
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           IF  WRK-PRODUTO-LISTA           GREATER ZEROS
               MOVE WRK-PRODUTO-LISTA      TO ENT5520-CPRODT
           END-IF.

           IF  WRK-SUBPRODUTO-LISTA        GREATER ZEROS
               MOVE WRK-SUBPRODUTO-LISTA   TO ENT5520-CSPROD-DESC-COML
           END-IF.

           IF  WRK-SITUACAO-LISTA          GREATER ZEROS
               MOVE WRK-SITUACAO-LISTA     TO ENT5520-CSIT-DESC-COML
           END-IF.

           IF  WRK-AGENCIA-LISTA           GREATER ZEROS
               MOVE WRK-AGENCIA-LISTA      TO ENT5520-CAG-BCRIA
                                              ENT5520-CAG-OPER-OPER
           END-IF.

           IF  WRK-INP-NMCONV-FITM         EQUAL 'CONV. CLIENTE'
               MOVE WRK-INP-CONVEN-FITM    TO ENT5520-CCONVE-CLI-DESC
               MOVE 9                      TO ENT5520-CELMTO-DESC-COML
           ELSE
               MOVE WRK-INP-CONVEN-FITM    TO ENT5520-CCONVE-GRAL-DESC
               MOVE 8                      TO ENT5520-CELMTO-DESC-COML
           END-IF.

           MOVE 'N'                        TO ENT5520-START-COUNT
           MOVE ZEROS                      TO ENT5520-FLAG
           MOVE ZEROS                      TO ENT5520-RESTART
           MOVE WRK-COD-USER               TO ENT5520-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT5520-CTERM
           MOVE WRK-COD-DEPTO-N            TO WRK-AGENCIA-NUM
DPJ22      IF   WRK-COD-USER(1:1) EQUAL 'M'
DPJ22           MOVE 4510                   TO WRK-AGENCIA-NUM
DPJ22      END-IF.
           MOVE WRK-AGENCIA-AUX            TO ENT5520-MSG-RETORNO

           IF   WRK-OPCAO                  EQUAL 1
                MOVE 'C1'                  TO ENT5520-FLAG-FILTRO
           ELSE
                MOVE 'H1'                  TO ENT5520-FLAG-FILTRO
           END-IF

           MOVE 237                        TO ENT5520-CBCO
           COMPUTE ENT5520-RESTART = (WRK-INP-PAGINA-FITM - 2) * 10

           MOVE 'DCOM5520'                 TO WRK-MODULO
           CALL WRK-MODULO             USING  ENTRADA-ROT5520
                                              SAIDA-ROT5520
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI5520-COD-RETORNO
             WHEN '0000'
             WHEN '0002'
               IF (SAI5520-FLAG                EQUAL 'S')
                   MOVE '*'                TO WRK-OUT-ULTPAG-FITM
                   MOVE SG-MENSAGEM(17)
                                           TO WRK-OUT-MENSA-FITM
               ELSE
                  MOVE SPACES              TO WRK-OUT-ULTPAG-FITM
                  MOVE SG-MENSAGEM(01)     TO WRK-OUT-MENSA-FITM
               END-IF

               PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                          UNTIL WRK-IND GREATER 10

             WHEN '0003'
             WHEN '0100'
                MOVE '*'                   TO WRK-OUT-ULTPAG-FITM
                MOVE SG-MENSAGEM(63)       TO WRK-OUT-MENSA-FITM

                PERFORM 2115-LIMPAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 10

             WHEN '0099'
               MOVE 'DB2'                  TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
                MOVE SAI5520-MSG-RETORNO   TO WRK-OUT-MENSA-FITM
                PERFORM 2210-DEVOLVER-TELA-VARIA

           END-EVALUATE.

           COMPUTE WRK-OUT-PAGINA-FITM = WRK-INP-PAGINA-FITM - 1.

      *----------------------------------------------------------------*
       2250-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     AVANCAR PARA TELA POSTERIOR                                *
      *----------------------------------------------------------------*
       2260-TELA-POSTERIOR                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-ULTPAG-FITM         EQUAL '*'
               IF  WRK-INP-OPER-FITM-X(1)  NOT NUMERIC OR
                   WRK-INP-OPER-FITM-X(1)  EQUAL SPACES
                   MOVE WRK-49345          TO WRK-OUT-COMANDO-ATTR-FITM
               END-IF
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(29)        TO WRK-OUT-MENSA-FITM
               GO TO 2260-99-FIM
           END-IF.

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      ENTRADA-ROT5520.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FITM       TO WRK-ENT-TIMESTAMP
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           IF  WRK-PRODUTO-LISTA           GREATER ZEROS
               MOVE WRK-PRODUTO-LISTA      TO ENT5520-CPRODT
           END-IF.

           IF  WRK-SUBPRODUTO-LISTA        GREATER ZEROS
               MOVE WRK-SUBPRODUTO-LISTA   TO ENT5520-CSPROD-DESC-COML
           END-IF.

           IF  WRK-SITUACAO-LISTA          GREATER ZEROS
               MOVE WRK-SITUACAO-LISTA     TO ENT5520-CSIT-DESC-COML
           END-IF.

           IF  WRK-AGENCIA-LISTA           GREATER ZEROS
               MOVE WRK-AGENCIA-LISTA      TO ENT5520-CAG-BCRIA
                                              ENT5520-CAG-OPER-OPER
           END-IF.

           IF  WRK-INP-NMCONV-FITM         EQUAL 'CONV. CLIENTE'
               MOVE WRK-INP-CONVEN-FITM    TO ENT5520-CCONVE-CLI-DESC
               MOVE 9                      TO ENT5520-CELMTO-DESC-COML
           ELSE
               MOVE WRK-INP-CONVEN-FITM    TO ENT5520-CCONVE-GRAL-DESC
               MOVE 8                      TO ENT5520-CELMTO-DESC-COML
           END-IF.

           MOVE 'N'                        TO ENT5520-START-COUNT
           MOVE ZEROS                      TO ENT5520-FLAG
           MOVE ZEROS                      TO ENT5520-RESTART
           MOVE WRK-COD-USER               TO ENT5520-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT5520-CTERM
           MOVE 237                        TO ENT5520-CBCO
           MOVE WRK-COD-DEPTO-N            TO WRK-AGENCIA-NUM
DPJ22      IF   WRK-COD-USER(1:1) EQUAL 'M'
DPJ22           MOVE 4510                   TO WRK-AGENCIA-NUM
DPJ22      END-IF.
           MOVE WRK-AGENCIA-AUX            TO ENT5520-MSG-RETORNO

           IF   WRK-OPCAO                  EQUAL 1
                MOVE 'C1'                  TO ENT5520-FLAG-FILTRO
           ELSE
                MOVE 'H1'                  TO ENT5520-FLAG-FILTRO
           END-IF

           COMPUTE ENT5520-RESTART = (WRK-INP-PAGINA-FITM) * 10

           MOVE 'DCOM5520'                 TO WRK-MODULO
           CALL WRK-MODULO             USING  ENTRADA-ROT5520
                                              SAIDA-ROT5520
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI5520-COD-RETORNO
             WHEN '0000'
             WHEN '0002'
               IF (SAI5520-FLAG            EQUAL 'S')
                   MOVE '*'                TO WRK-OUT-ULTPAG-FITM
                   MOVE SG-MENSAGEM(17)    TO WRK-OUT-MENSA-FITM
               ELSE
                   MOVE SPACES             TO WRK-OUT-ULTPAG-FITM
                   MOVE SG-MENSAGEM(01)    TO WRK-OUT-MENSA-FITM
               END-IF

               PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                          UNTIL WRK-IND GREATER 10

             WHEN '0003'
             WHEN '0100'
                MOVE '*'                   TO WRK-OUT-ULTPAG-FITM
                MOVE SG-MENSAGEM(63)       TO WRK-OUT-MENSA-FITM

                PERFORM 2115-LIMPAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 10

             WHEN '0099'
               MOVE 'DB2'                  TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
                MOVE SAI5520-MSG-RETORNO   TO WRK-OUT-MENSA-FITM
                PERFORM 2210-DEVOLVER-TELA-VARIA

           END-EVALUATE.

           COMPUTE WRK-OUT-PAGINA-FITM = WRK-INP-PAGINA-FITM + 1.

      *----------------------------------------------------------------*
       2260-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2270-TRATAR-ENTER                   SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                    TO WRK-INCONSIS

           PERFORM 2271-CONSISTIR-FISICA.

      *----------------------------------------------------------------*
       2270-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-OPER-FITM-X(1)    NOT NUMERIC OR
               WRK-INP-OPER-FITM-X(1)    EQUAL SPACES
               MOVE WRK-49345            TO WRK-OUT-COMANDO-ATTR-FITM
               MOVE 'NAO FORAM ENCONTRADOS DADOS PARA ESTE ARGUMENTO DE
      -             'PESQUISA'
                                         TO WRK-OUT-MENSA-FITM
               GO TO 2271-99-FIM
           END-IF

           MOVE ZEROS                      TO WCT-SELEC

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 10

               IF  WRK-INP-SELEC-FITM(WRK-IND) NOT EQUAL SPACE AND 'X'
                   MOVE '*'                TO WRK-INCONSIS
                   MOVE WRK-49369          TO
                        WRK-OUT-SELEC-ATTR-FITM(WRK-IND)
                   MOVE SG-MENSAGEM(41)    TO WRK-OUT-MENSA-FITM
               ELSE
                   IF  WRK-INP-SELEC-FITM(WRK-IND) EQUAL 'X'
                       MOVE WRK-49369      TO
                            WRK-OUT-SELEC-ATTR-FITM(WRK-IND)
                       ADD 1               TO WCT-SELEC
                       MOVE WRK-IND        TO WIND-SEL
                   END-IF
               END-IF
           END-PERFORM.

           IF WRK-INCONSIS  EQUAL  '*'
              GO TO 2271-99-FIM
           END-IF.

           IF  WCT-SELEC                   EQUAL 0  OR  GREATER 1
               MOVE '*'                    TO WRK-INCONSIS
               IF  WCT-SELEC               EQUAL 0
                   MOVE SG-MENSAGEM(53)    TO WRK-OUT-MENSA-FITM
               ELSE
                 IF  WCT-SELEC             GREATER 1
                     MOVE SG-MENSAGEM(39)  TO WRK-OUT-MENSA-FITM
                 END-IF
               END-IF
           ELSE
               PERFORM 2272-TRANSFERIR-CONTROLE
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2272-TRANSFERIR-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-IMS.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FITM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)      EQUAL SPACES
                   MOVE 'DCOM0609'          TO WRK-CHAMADOR(IND-1)
                                               COMU-TRANSACAO
                   MOVE WRK-INP-PAGINA-FITM TO WRK-PAGINA(IND-1)
                   MOVE 99                  TO IND-1
               END-IF
           END-PERFORM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 10

               IF  WRK-INP-SELEC-FITM(WRK-IND) EQUAL 'X'
                   MOVE WRK-INP-OPER-FITM(WRK-IND)(1:4)
                                    TO WRK-DANO-OPER-DESC
                   MOVE WRK-INP-OPER-FITM(WRK-IND)(5:9)
                                    TO WRK-NSEQ-OPER-DESC
               END-IF
           END-PERFORM.

           IF  WRK-OPCAO                EQUAL 1
               MOVE 'DCOM0612'          TO WRK-TELA
           ELSE
               MOVE 'DCOM0617'          TO WRK-TELA
           END-IF

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-CHNG                   TO WRK-FUNCAO.
           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL.
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2272-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660             SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FITM
                                       TO  WRK-660-LL-MENS-FITM
                                           WRK-OUT-LL-FITM

           MOVE LENGTH OF WRK-660-DCOMFITM
                                       TO  WRK-660-LL-AREA-FITM

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FITM
                                              WRK-660-DCOMFITM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-FITM            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3000-FINALIZAR                      SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       7000-ACESSAR-DCOM5520               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      ENTRADA-ROT5520.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FITM       TO WRK-ENT-TIMESTAMP
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           IF  WRK-PRODUTO-LISTA           GREATER ZEROS
               MOVE WRK-PRODUTO-LISTA      TO ENT5520-CPRODT
           END-IF.

           IF  WRK-SUBPRODUTO-LISTA        GREATER ZEROS
               MOVE WRK-SUBPRODUTO-LISTA   TO ENT5520-CSPROD-DESC-COML
           END-IF.

           IF  WRK-SITUACAO-LISTA          GREATER ZEROS
               MOVE WRK-SITUACAO-LISTA     TO ENT5520-CSIT-DESC-COML
           END-IF.

           IF  WRK-AGENCIA-LISTA           GREATER ZEROS
               MOVE WRK-AGENCIA-LISTA      TO ENT5520-CAG-BCRIA
                                              ENT5520-CAG-OPER-OPER
           END-IF.

           IF  WRK-CONVENIO-CLIENTE        GREATER ZEROS
               MOVE WRK-CONVENIO-CLIENTE   TO ENT5520-CCONVE-CLI-DESC
               MOVE 9                      TO ENT5520-CELMTO-DESC-COML
           ELSE
               MOVE WRK-CONVENIO-GERAL     TO ENT5520-CCONVE-GRAL-DESC
               MOVE 8                      TO ENT5520-CELMTO-DESC-COML
           END-IF.

           MOVE 'N'                        TO ENT5520-START-COUNT
           MOVE ZEROS                      TO ENT5520-FLAG
           MOVE WRK-COD-USER               TO ENT5520-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT5520-CTERM
           MOVE 237                        TO ENT5520-CBCO
           MOVE WRK-COD-DEPTO-N            TO WRK-AGENCIA-NUM
DPJ22      IF   WRK-COD-USER(1:1) EQUAL 'M'
DPJ22           MOVE 4510                   TO WRK-AGENCIA-NUM
DPJ22      END-IF.
           MOVE WRK-AGENCIA-AUX            TO ENT5520-MSG-RETORNO

           IF   WRK-OPCAO                  EQUAL 1
                MOVE 'C1'                  TO ENT5520-FLAG-FILTRO
           ELSE
                MOVE 'H1'                  TO ENT5520-FLAG-FILTRO
           END-IF


           MOVE 'DCOM5520'                 TO WRK-MODULO

           CALL WRK-MODULO             USING  ENTRADA-ROT5520
                                              SAIDA-ROT5520
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI5520-COD-RETORNO
              WHEN '0000'
              WHEN '0002'
               IF  (SAI5520-FLAG              EQUAL 'S')
                   MOVE '*'                   TO WRK-OUT-ULTPAG-FITM
                   IF (SAI5520-QTDE-RETORNADA EQUAL ZEROS)
                       MOVE WRK-49345       TO WRK-OUT-COMANDO-ATTR-FITM
                       MOVE SG-MENSAGEM(63)   TO WRK-OUT-MENSA-FITM
                    ELSE
                       MOVE SG-MENSAGEM(61)   TO WRK-OUT-MENSA-FITM
                    END-IF
                ELSE
                    MOVE SPACES            TO WRK-OUT-ULTPAG-FITM
                    MOVE SG-MENSAGEM(61)   TO WRK-OUT-MENSA-FITM
                END-IF

                PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 10

              WHEN '0003'
              WHEN '0100'
                MOVE WRK-49345             TO WRK-OUT-COMANDO-ATTR-FITM
                MOVE '*'                   TO WRK-OUT-ULTPAG-FITM
                MOVE 'NAO FORAM ENCONTRADOS DADOS PARA ESTE ARGUMENTO DE
      -              ' PESQUISA'
                                           TO WRK-OUT-MENSA-FITM

                PERFORM 2220-RETORNAR-CHAMADOR


              WHEN '0099'
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                MOVE WRK-49345             TO WRK-OUT-COMANDO-ATTR-FITM
                MOVE SAI5520-MSG-RETORNO   TO WRK-OUT-MENSA-FITM

                PERFORM 2220-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       7000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO          SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0609'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0609'             TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER               TO ERR-COD-USER
           MOVE WRK-COD-DEPTO              TO ERR-COD-DEPTO

           IF  ERR-TIPO-ACESSO             EQUAL 'DB2'
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
                                              WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
           END-IF.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
