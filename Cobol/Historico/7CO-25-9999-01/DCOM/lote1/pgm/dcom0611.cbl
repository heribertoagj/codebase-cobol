      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM0611.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0611                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: NELSON TANAKA - ALTRAN.                      *
      *     DATA........: 21/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTA CONSULTA OPERACAO - OPERACES P/ CLIENTE*
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMFKTM....: LISTA CONSULTA OPERACAO - OPERACOES P/ CONV. *
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
      *     CHAMADO POR.: DCOM0603                                     *
      *================================================================*
DPJ22 *                    ALTERACAO                                   *
DPJ22 *----------------------------------------------------------------*
DPJ22 *    PROGRAMADOR.:   LEANDRO TAKAOKA           - BRADESCO        *
DPJ22 *    DATA........:   16/09/2025                                  *
DPJ22 *----------------------------------------------------------------*
DPJ22 *    OBJETIVO....: LIBERAR ACESSO A TERCEIROS  DLVRPJ-22         *
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
           '*** DCOM0611 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  IND-1                           PIC S9(09) COMP VALUE ZEROS.
       77  WRK-IND                         PIC S9(09) COMP VALUE ZEROS.
       77  WIND-SEL                        PIC S9(09) COMP VALUE ZEROS.
       77  WCT-SELEC                       PIC S9(09) COMP VALUE ZEROS.
       77  WRK-VOPER-E                     PIC ZZZ.ZZZ.ZZ9,99.
       77  WRK-MENSA-ERRO                  PIC  X(79) VALUE SPACES.
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
           05  WRK-CNPJ-TELA           PIC  9(009)     VALUE ZEROS.
           05  FILLER                  PIC  X(001)     VALUE '/'.
           05  WRK-FILIAL-TELA         PIC  9(004)     VALUE ZEROS.
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
           '*** AREA DA TELA DCOMFKTM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMFKTM.
           05  WRK-660-LL-AREA-FKTM        PIC  9(04) COMP VALUE 0188.
           05  WRK-660-LL-MENS-FKTM        PIC  9(04) COMP VALUE 1022.
           05  WRK-660-SENHAS-FKTM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FKTM        PIC  9(04) COMP VALUE 2070.
           05  WRK-660-TMSTAMP-FKTM        PIC  9(04) COMP VALUE 0026.
           05  WRK-660-ULTPAG-FKTM         PIC  9(04) COMP VALUE 0001.
           05  WRK-660-PAGINA-FKTM         PIC  9(04) COMP VALUE 0004.
           05  WRK-660-CCONTR-FKTM         PIC  9(04) COMP VALUE 0009.
           05  WRK-660-DPROD-FKTM          PIC  9(04) COMP VALUE 0016.
           05  WRK-660-SPROD-FKTM          PIC  9(04) COMP VALUE 0014.
           05  WRK-660-AGENC-FKTM          PIC  9(04) COMP VALUE 0020.
           05  WRK-660-SITUA-FKTM          PIC  9(04) COMP VALUE 0012.
           05  WRK-600-LISTA               OCCURS  10.
               10 WRK-660-SEL-FKTM         PIC  9(04) COMP VALUE 2003.
               10 WRK-660-OPER-FKTM        PIC  9(04) COMP VALUE 0013.
               10 WRK-660-PROD-FKTM        PIC  9(04) COMP VALUE 0006.
               10 WRK-660-SUBP-FKTM        PIC  9(04) COMP VALUE 0006.
               10 WRK-660-NOME-FKTM        PIC  9(04) COMP VALUE 0010.
               10 WRK-660-DINC-FKTM        PIC  9(04) COMP VALUE 0010.
               10 WRK-660-VOPE-FKTM        PIC  9(04) COMP VALUE 0014.
               10 WRK-660-SITU-FKTM        PIC  9(04) COMP VALUE 0006.
               10 WRK-660-AGE-FKTM         PIC  9(04) COMP VALUE 0005.
           05  WRK-660-MENSA-FKTM          PIC  9(04) COMP VALUE 0079.
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
           '*** AREA DA TELA DCOMFKTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-FKTM.
           05  FILLER                      PIC  X(18)      VALUE SPACES.
           05  WRK-INP-PFK-FKTM            PIC  X(01)      VALUE SPACES.
           05  WRK-INP-SENHAS-FKTM         PIC  X(37)      VALUE SPACES.
           05  WRK-INP-COMANDO-FKTM        PIC  X(68)      VALUE SPACES.
           05  WRK-INP-DADOS-FKTM.
               10  WRK-INP-TMSTAMP-FKTM    PIC  X(26)      VALUE SPACES.
               10  WRK-INP-ULTPAG-FKTM     PIC  X(01)      VALUE SPACE.
               10  WRK-INP-PAGINA-FKTM-X.
                   15  WRK-INP-PAGINA-FKTM
                                           PIC  9(04)      VALUE ZEROS.
      *
               10  WRK-INP-CCONTR-FKTM     PIC  X(09)      VALUE SPACES.
               10  WRK-INP-DPROD-FKTM      PIC  X(16)      VALUE SPACES.
               10  WRK-INP-SPROD-FKTM      PIC  X(14)      VALUE SPACES.
               10  WRK-INP-AGENC-FKTM      PIC  X(20)      VALUE SPACES.
               10  WRK-INP-SITUA-FKTM      PIC  X(12)      VALUE SPACES.
      *
               10  WRK-INP-LINHAS-FKTM OCCURS 10 TIMES.
                   15  WRK-INP-SELEC-FKTM
                                           PIC  X(01)      VALUE SPACES.
                   15  WRK-INP-OPER-FKTM-X.
                       20  WRK-INP-OPER-FKTM
                                           PIC  9(13)      VALUE ZEROS.
                   15  WRK-INP-PROD-FKTM   PIC  X(06)      VALUE SPACES.
                   15  WRK-INP-SUBP-FKTM   PIC  X(06)      VALUE SPACES.
                   15  WRK-INP-NOME-FKTM   PIC  X(10)      VALUE SPACES.
                   15  WRK-INP-DINC-FKTM   PIC  X(10)      VALUE SPACES.
                   15  WRK-INP-VOPE-FKTM   PIC  X(14)      VALUE SPACES.
                   15  WRK-INP-SITU-FKTM   PIC  X(06)      VALUE SPACES.
                   15  WRK-INP-AGE-FKTM    PIC  X(05)      VALUE SPACES.
           05  WRK-INP-MENSA-FKTM          PIC  X(79)      VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFKTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-FKTM.
           05  WRK-OUT-LL-FKTM             PIC S9(04) COMP VALUE +1022.
           05  WRK-OUT-ZZ-FKTM             PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FKTM         PIC  X(37)      VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-FKTM   PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-COMANDO-FKTM        PIC  X(68)      VALUE SPACES.
           05  WRK-OUT-DADOS-FKTM.
               10  WRK-OUT-TMSTAMP-FKTM    PIC  X(26)      VALUE SPACES.
               10  WRK-OUT-ULTPAG-FKTM     PIC  X(01)      VALUE SPACE.
               10  WRK-OUT-PAGINA-FKTM-X.
                   15  WRK-OUT-PAGINA-FKTM PIC  9(04)      VALUE ZEROS.
      *
               10  WRK-OUT-CCONTR-FKTM     PIC  X(09)      VALUE SPACES.
               10  WRK-OUT-DPROD-FKTM      PIC  X(16)      VALUE SPACES.
               10  WRK-OUT-SPROD-FKTM      PIC  X(14)      VALUE SPACES.
               10  WRK-OUT-AGENC-FKTM      PIC  X(20)      VALUE SPACES.
               10  WRK-OUT-SITUA-FKTM      PIC  X(12)      VALUE SPACES.
      *
               10  WRK-OUT-LINHAS-FKTM OCCURS 10 TIMES.
                   15  WRK-OUT-SELEC-ATTR-FKTM
                                           PIC S9(04) COMP VALUE ZEROS.
                   15  WRK-OUT-SELEC-FKTM  PIC  X(01)      VALUE SPACES.
                   15  WRK-OUT-OPER-FKTM-X.
                       20  WRK-OUT-OPER-FKTM
                                           PIC  9(13)      VALUE ZEROS.
                   15  WRK-OUT-PROD-FKTM   PIC  X(06)      VALUE SPACES.
                   15  WRK-OUT-SUBP-FKTM   PIC  X(06)      VALUE SPACES.
                   15  WRK-OUT-NOME-FKTM   PIC  X(10)      VALUE SPACES.
                   15  WRK-OUT-DINC-FKTM   PIC  X(10)      VALUE SPACES.
                   15  WRK-OUT-VOPE-FKTM   PIC  X(14)      VALUE SPACES.
                   15  WRK-OUT-SITU-FKTM   PIC  X(06)      VALUE SPACES.
                   15  WRK-OUT-AGE-FKTM    PIC  X(05)      VALUE SPACES.
           05  WRK-OUT-MENSA-FKTM          PIC  X(79)      VALUE SPACES.

      *----------------------------------------------------------------*
      * I#DCOMX3                                                       *
      * AREA DE COMUNICACAO COM O  MODULO  DCOMXXXX P/ SELECAO DE      *
      *         CONTAS E  PESSOAS CADASTRADAS NO CADU
      *----------------------------------------------------------------*
       COPY 'I#DCOMX3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4128 ***'.
      *----------------------------------------------------------------*
       01  4128E-ENTRADA.
           03  4128E-COD-RETORNO       PIC  X(04).
           03  4128E-MSG-RETORNO       PIC  X(79).
           03  4128E-RESTART           PIC  9(05).
           03  4128E-FLAG              PIC  X(01).
           03  4128E-CFUNC-BDSCO       PIC  9(09).
           03  4128E-CTERM             PIC  X(08).
           03  4128E-CCONTR-LIM-DESC   PIC  9(09).
           03  4128E-CVRSAO-CONTR-LIM  PIC  9(03).
           03  4128E-FLAG-FILTRO       PIC  X(01).
       01  4128S-SAIDA.
           03  4128S-HEADER.
               05  4128S-COD-RETORNO             PIC  X(04).
               05  4128S-MSG-RETORNO             PIC  X(79).
               05  4128S-RESTART                 PIC  9(05).
               05  4128S-FLAG                    PIC  X(01).
           03  4128S-DADOS.
               05  4128S-CPRODT                   PIC  9(03).
               05  4128S-ISIT-DESC-COML           PIC  X(30).
               05  4128S-IRSUMO-SIT-DESC          PIC  X(15).
FS2511*        05  4128S-CCNPJ-CPF                PIC  9(09).
FS2511         05  4128S-CCNPJ-CPF                PIC  X(009).
FS2511*        05  4128S-CFLIAL-CNPJ              PIC  9(04).
FS2511         05  4128S-CFLIAL-CNPJ              PIC  X(04).
               05  4128S-CCTRL-CNPJ               PIC  9(02).
               05  4128S-IPSSOA-DESC-COML         PIC  X(60).
               05  4128S-CBCO                     PIC  9(03).
               05  4128S-CBCO-DESC                PIC  X(20).
               05  4128S-CAG-BCRIA                PIC  9(05).
               05  4128S-CAG-BCRIA-DESC           PIC  X(30).
               05  4128S-CCTA-BCRIA-CLI           PIC  9(13).
               05  4128S-DINIC-VGCIA-CONTR        PIC  X(10).
               05  4128S-CINDCD-RENOV-AUTOM       PIC  X(01).
               05  4128S-DVCTO-CONTR-LIM          PIC  X(10).
               05  4128S-TCONTR-LIM-DESC          PIC  9(05).
               05  4128S-VCONTR-LIM-DESC          PIC  9(15)V99.
               05  4128S-VDISPN-CONTR-LIM         PIC  9(15)V99.
               05  4128S-CTPO-COBR-TAC            PIC  9(03).
               05  4128S-ITPO-COBR-TAC            PIC  X(30).
               05  4128S-VTAC-CONTR-LIM           PIC  9(15)V99.
               05  4128S-PARECER                  PIC  X(240).
               05  4128S-CGARAN                   PIC  9(003).
               05  4128S-IGARAN                   PIC  X(040).
               05  4128S-VUTILZ-CONTR-LIM         PIC  9(15)V99.

           03  4128S-CONSISTENCIA REDEFINES 4128S-DADOS.
               05  4128S-TAMANHO-ERRO             PIC  9(01).
               05  4128S-TABELA-ERROS             OCCURS 01 TIMES.
                   07  4128S-CAMPOS-ERRO          PIC  9(01).


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
           '*** DCOM0611 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                     TO WRK-OUT-DADOS-FKTM
                                              WRK-OUT-SENHAS-FKTM
                                              WRK-OUT-COMANDO-FKTM
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

           MOVE 'DCOMFKTM'                 TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0611'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFKTM
           END-IF.

           IF  WRK-TELA                    EQUAL 'DCOMFKTM'
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
           MOVE COMU-TIMESTAMP             TO WRK-OUT-TMSTAMP-FKTM
                                              WRK-INP-TMSTAMP-FKTM
                                              WRK-ENT-TIMESTAMP.

           MOVE 1                          TO WRK-ENT-PAGINA
                                              WRK-OUT-PAGINA-FKTM.
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL


           MOVE WRK-CONTRATO              TO WRK-INP-CCONTR-FKTM
                                             WRK-OUT-CCONTR-FKTM.

           IF  WRK-AGENCIA-LISTA           EQUAL ZEROS
               MOVE SPACES                 TO WRK-INP-AGENC-FKTM
                                              WRK-OUT-AGENC-FKTM
           ELSE
              STRING WRK-AGENCIA-LISTA '-' WRK-DESC-AGENCIA
                DELIMITED BY SIZE
                INTO WRK-INP-AGENC-FKTM
              MOVE WRK-INP-AGENC-FKTM      TO WRK-OUT-AGENC-FKTM
           END-IF.

           IF  WRK-PRODUTO-LISTA    EQUAL ZEROS
               MOVE SPACES                 TO WRK-INP-DPROD-FKTM
                                              WRK-OUT-DPROD-FKTM
           ELSE
              STRING WRK-PRODUTO-LISTA '-' WRK-DESC-PRODUTO
                DELIMITED BY SIZE
                INTO WRK-INP-DPROD-FKTM
              MOVE WRK-INP-DPROD-FKTM       TO WRK-OUT-DPROD-FKTM
           END-IF.

           IF  WRK-SUBPRODUTO-LISTA EQUAL ZEROS
               MOVE SPACES                 TO WRK-INP-SPROD-FKTM
                                              WRK-OUT-SPROD-FKTM
           ELSE
              STRING WRK-SUBPRODUTO-LISTA '-' WRK-DESC-SPROD
                DELIMITED BY SIZE
                INTO WRK-INP-SPROD-FKTM
              MOVE WRK-INP-SPROD-FKTM      TO WRK-OUT-SPROD-FKTM
           END-IF.

           PERFORM 2105-BUSCA-SITUACAO

           IF WRK-INCONSIS          EQUAL '*'
              GO TO 2100-99-FIM
           END-IF

           INITIALIZE  ENTRADA-ROT5520
                       SAIDA-ROT5520

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15

               IF  WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0611'
                   COMPUTE ENT5520-RESTART = (WRK-PAGINA(IND-1) - 1)
                                             * 10
                   MOVE WRK-PAGINA(IND-1)    TO WRK-OUT-PAGINA-FKTM
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
           MOVE WRK-INP-TMSTAMP-FKTM       TO WRK-ENT-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           PERFORM 7000-ACESSAR-DCOM5520.

           IF  SAI5520-COD-RETORNO      NOT EQUAL '0000' AND '0002'
               MOVE '*'                 TO WRK-INCONSIS
               MOVE SAI5520-MSG-RETORNO TO WRK-MENSA-ERRO
               PERFORM 2220-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2105-BUSCA-SITUACAO             SECTION.
      *----------------------------------------------------------------*
           INITIALIZE    4128E-ENTRADA
                         4128S-SAIDA.

           MOVE WRK-COD-USER           TO 4128E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4128E-CTERM
           MOVE ZEROS                  TO 4128E-RESTART
           MOVE SPACES                 TO 4128E-FLAG
           MOVE WRK-CONTRATO           TO 4128E-CCONTR-LIM-DESC
           MOVE 'H'                    TO 4128E-FLAG-FILTRO

           MOVE 'DCOM4128'             TO WRK-MODULO.
           CALL WRK-MODULO             USING 4128E-ENTRADA
                                             4128S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.
           EVALUATE 4128S-COD-RETORNO

               WHEN  '0000'
                   MOVE 4128S-IRSUMO-SIT-DESC    TO WRK-INP-SITUA-FKTM
                                                    WRK-OUT-SITUA-FKTM

               WHEN '0099'
                   MOVE 'DB2'                    TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE '*'                      TO WRK-INCONSIS
                   MOVE 4128S-MSG-RETORNO        TO WRK-MENSA-ERRO
                   PERFORM 2220-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       2105-99-FIM.                        EXIT.
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
                   WRK-OUT-SELEC-ATTR-FKTM(WRK-IND)
           ELSE
               MOVE SAI5520-DANO-OPER-DESC(WRK-IND)
                                           TO
                    WRK-OUT-OPER-FKTM(WRK-IND)(01:04)
               MOVE SAI5520-NSEQ-OPER-DESC(WRK-IND)
                                           TO
                    WRK-OUT-OPER-FKTM(WRK-IND)(05:09)
               MOVE SAI5520-CSGL-PRODT-DESC(WRK-IND)
                                           TO WRK-OUT-PROD-FKTM(WRK-IND)
               MOVE SAI5520-CSGL-SPROD-DESC(WRK-IND)
                                           TO WRK-OUT-SUBP-FKTM(WRK-IND)
               MOVE SAI5520-IPSSOA-DESC-COML(WRK-IND)
                                           TO WRK-OUT-NOME-FKTM(WRK-IND)
               MOVE SAI5520-CAG-OPER-OPER(WRK-IND)
                                           TO WRK-OUT-AGE-FKTM(WRK-IND)
               MOVE SAI5520-DINIC-OPER-DESC(WRK-IND)
                                           TO WRK-OUT-DINC-FKTM(WRK-IND)
               INSPECT WRK-OUT-DINC-FKTM(WRK-IND)
                                           REPLACING ALL '.' BY '/'
               MOVE SAI5520-VOPER-DESC-COML(WRK-IND)
                                           TO WRK-VOPER-E
               MOVE WRK-VOPER-E            TO WRK-OUT-VOPE-FKTM(WRK-IND)
               MOVE SAI5520-IRSUMO-SIT-DESC(WRK-IND)
                                           TO WRK-OUT-SITU-FKTM(WRK-IND)
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
               WRK-OUT-SELEC-ATTR-FKTM(WRK-IND)
           MOVE SPACES                     TO
                WRK-OUT-OPER-FKTM-X(WRK-IND)

           MOVE SPACES                   TO WRK-OUT-PROD-FKTM(WRK-IND)
                                            WRK-OUT-SUBP-FKTM(WRK-IND)
                                            WRK-OUT-NOME-FKTM(WRK-IND)
                                            WRK-OUT-DINC-FKTM(WRK-IND)
                                            WRK-OUT-VOPE-FKTM(WRK-IND)
                                            WRK-OUT-SITU-FKTM(WRK-IND)
                                            WRK-OUT-AGE-FKTM (WRK-IND).

      *----------------------------------------------------------------*
       2115-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOMFKTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFKTM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FKTM

           PERFORM 2205-DEVOLVER-TELA-FIXO

           EVALUATE TRUE
             WHEN WRK-INP-PFK-FKTM         EQUAL 'H' OR
                  (WRK-INP-PFK-FKTM        EQUAL '.' AND
                   WRK-INP-COMANDO-FKTM    EQUAL 'PFK01')
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   IF  WRK-INP-OPER-FKTM-X(1)  NOT NUMERIC OR
                       WRK-INP-OPER-FKTM-X(1)  EQUAL SPACES
                       MOVE WRK-49345      TO WRK-OUT-COMANDO-ATTR-FKTM
                   END-IF
                   MOVE WRK-INP-MENSA-FKTM TO  WRK-OUT-MENSA-FKTM

             WHEN WRK-INP-PFK-FKTM         EQUAL '3' OR
                   (WRK-INP-PFK-FKTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FKTM   EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FKTM       EQUAL 'X' OR
                   (WRK-INP-PFK-FKTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FKTM   EQUAL 'PFK05')
                  PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-FKTM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FKTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FKTM   EQUAL 'PFK10')
                  PERFORM 2240-MENU-DCOM

             WHEN WRK-INP-PFK-FKTM         EQUAL '7' OR
                 (WRK-INP-PFK-FKTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FKTM     EQUAL 'PFK07')
                  PERFORM 2250-TELA-ANTERIOR

             WHEN WRK-INP-PFK-FKTM         EQUAL '8' OR
                 (WRK-INP-PFK-FKTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FKTM     EQUAL 'PFK08')
                  PERFORM 2260-TELA-POSTERIOR

             WHEN (WRK-INP-PFK-FKTM        EQUAL '.' AND
                   WRK-INP-COMANDO-FKTM    EQUAL SPACES)
                   PERFORM 2270-TRATAR-ENTER
                   PERFORM 2210-DEVOLVER-TELA-VARIA

             WHEN  OTHER
                   IF  WRK-INP-OPER-FKTM-X(1)  NOT NUMERIC OR
                       WRK-INP-OPER-FKTM-X(1)  EQUAL SPACES
                       MOVE WRK-49345      TO WRK-OUT-COMANDO-ATTR-FKTM
                   END-IF
                   MOVE SG-MENSAGEM(38)    TO  WRK-OUT-MENSA-FKTM
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

           MOVE WRK-INP-TMSTAMP-FKTM       TO WRK-OUT-TMSTAMP-FKTM
           MOVE WRK-INP-ULTPAG-FKTM        TO WRK-OUT-ULTPAG-FKTM
           MOVE WRK-INP-PAGINA-FKTM-X      TO WRK-OUT-PAGINA-FKTM-X
           MOVE WRK-INP-CCONTR-FKTM        TO WRK-OUT-CCONTR-FKTM
           MOVE WRK-INP-DPROD-FKTM         TO WRK-OUT-DPROD-FKTM
           MOVE WRK-INP-SPROD-FKTM         TO WRK-OUT-SPROD-FKTM
           MOVE WRK-INP-AGENC-FKTM         TO WRK-OUT-AGENC-FKTM
           MOVE WRK-INP-SITUA-FKTM         TO WRK-OUT-SITUA-FKTM
           MOVE WRK-INP-MENSA-FKTM         TO WRK-OUT-MENSA-FKTM.

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

               IF (WRK-INP-OPER-FKTM(WRK-IND) NOT NUMERIC) OR
                  (WRK-INP-OPER-FKTM(WRK-IND) EQUAL ZEROS)
                   MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-FKTM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-FKTM(WRK-IND)
                                     TO WRK-OUT-SELEC-FKTM(WRK-IND)
                   MOVE WRK-INP-OPER-FKTM-X(WRK-IND)
                                     TO WRK-OUT-OPER-FKTM-X(WRK-IND)
                   MOVE WRK-INP-PROD-FKTM(WRK-IND)
                                     TO WRK-OUT-PROD-FKTM(WRK-IND)
                   MOVE WRK-INP-SUBP-FKTM(WRK-IND)
                                     TO WRK-OUT-SUBP-FKTM(WRK-IND)
                   MOVE WRK-INP-NOME-FKTM(WRK-IND)
                                     TO WRK-OUT-NOME-FKTM(WRK-IND)
                   MOVE WRK-INP-DINC-FKTM(WRK-IND)
                                     TO WRK-OUT-DINC-FKTM(WRK-IND)
                   MOVE WRK-INP-VOPE-FKTM(WRK-IND)
                                     TO WRK-OUT-VOPE-FKTM(WRK-IND)
                   MOVE WRK-INP-SITU-FKTM(WRK-IND)
                                     TO WRK-OUT-SITU-FKTM(WRK-IND)
                   MOVE WRK-INP-AGE-FKTM (WRK-IND)
                                     TO WRK-OUT-AGE-FKTM (WRK-IND)
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

           MOVE WRK-INP-TMSTAMP-FKTM       TO WRK-ENT-TIMESTAMP
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
           END-PERFORM.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               MOVE WRK-CHAMADOR(IND-1)    TO WRK-TELA
           END-PERFORM.

     **--  RETORNAR MSG ERRO DA 4128 (CONSULTA CONTRATO LIMITE)
           IF  WRK-INCONSIS         EQUAL '*'
               MOVE WRK-MENSA-ERRO         TO WRK-MENSAGEM-COMUM
           END-IF

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FKTM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE SPACES                     TO WRK-CHAMADOR(IND-1 - 1)
           MOVE ZEROS                      TO WRK-PAGINA(IND-1 - 1)
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE LENGTH COMU-AREA-IMS       TO COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0611'                 TO COMU-TRANSACAO
           MOVE '3'                        TO COMU-PFK
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
           END-PERFORM.

      *----------------------------------------------------------------*
       2220-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
       2230-MENU-ROTINAS                   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TMSTAMP-FKTM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0611'                 TO WRK-TELA
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

           MOVE WRK-INP-TMSTAMP-FKTM       TO WRK-ENT-TIMESTAMP.
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

           IF  WRK-INP-PAGINA-FKTM         EQUAL 1
               IF  WRK-INP-OPER-FKTM-X(1)  NOT NUMERIC OR
                   WRK-INP-OPER-FKTM-X(1)  EQUAL SPACES
                   MOVE WRK-49345          TO WRK-OUT-COMANDO-ATTR-FKTM
               END-IF
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(28)        TO WRK-OUT-MENSA-FKTM
               GO TO 2250-99-FIM
           END-IF.

           MOVE SPACES                    TO WRK-INP-ULTPAG-FKTM
                                             WRK-OUT-ULTPAG-FKTM.

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      ENTRADA-ROT5520.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FKTM       TO WRK-ENT-TIMESTAMP
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

           MOVE WRK-CONTRATO               TO ENT5520-CCONTR-LIM-DESC
           MOVE 10                         TO ENT5520-CELMTO-DESC-COML

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
           COMPUTE ENT5520-RESTART = (WRK-INP-PAGINA-FKTM - 2) * 10

           MOVE 'DCOM5520'                 TO WRK-MODULO
           CALL WRK-MODULO             USING  ENTRADA-ROT5520
                                              SAIDA-ROT5520
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI5520-COD-RETORNO
             WHEN '0000'
             WHEN '0002'
               IF (SAI5520-FLAG                EQUAL 'S')
                   MOVE '*'                TO WRK-OUT-ULTPAG-FKTM
                   MOVE SG-MENSAGEM(17)
                                           TO WRK-OUT-MENSA-FKTM
               ELSE
                  MOVE SPACES              TO WRK-OUT-ULTPAG-FKTM
                  MOVE SG-MENSAGEM(01)     TO WRK-OUT-MENSA-FKTM
               END-IF

               PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                          UNTIL WRK-IND GREATER 10

             WHEN '0003'
             WHEN '0100'
                MOVE '*'                   TO WRK-OUT-ULTPAG-FKTM
                MOVE SG-MENSAGEM(63)       TO WRK-OUT-MENSA-FKTM

                PERFORM 2115-LIMPAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 10

             WHEN '0099'
               MOVE 'DB2'                  TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
                MOVE SAI5520-MSG-RETORNO   TO WRK-OUT-MENSA-FKTM
                PERFORM 2210-DEVOLVER-TELA-VARIA

           END-EVALUATE.

           COMPUTE WRK-OUT-PAGINA-FKTM = WRK-INP-PAGINA-FKTM - 1.

      *----------------------------------------------------------------*
       2250-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     AVANCAR PARA TELA POSTERIOR                                *
      *----------------------------------------------------------------*
       2260-TELA-POSTERIOR                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-ULTPAG-FKTM         EQUAL '*'
               IF  WRK-INP-OPER-FKTM-X(1)  NOT NUMERIC OR
                   WRK-INP-OPER-FKTM-X(1)  EQUAL SPACES
                   MOVE WRK-49345          TO WRK-OUT-COMANDO-ATTR-FKTM
               END-IF
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(29)        TO WRK-OUT-MENSA-FKTM
               GO TO 2260-99-FIM
           END-IF.

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      ENTRADA-ROT5520.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FKTM       TO WRK-ENT-TIMESTAMP
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

           MOVE WRK-CONTRATO               TO ENT5520-CCONTR-LIM-DESC
           MOVE 10                         TO ENT5520-CELMTO-DESC-COML

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

           COMPUTE ENT5520-RESTART = (WRK-INP-PAGINA-FKTM) * 10

           MOVE 'DCOM5520'                 TO WRK-MODULO
           CALL WRK-MODULO             USING  ENTRADA-ROT5520
                                              SAIDA-ROT5520
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI5520-COD-RETORNO
             WHEN '0000'
             WHEN '0002'
               IF (SAI5520-FLAG            EQUAL 'S')
                   MOVE '*'                TO WRK-OUT-ULTPAG-FKTM
                   MOVE SG-MENSAGEM(17)    TO WRK-OUT-MENSA-FKTM
               ELSE
                   MOVE SPACES             TO WRK-OUT-ULTPAG-FKTM
                   MOVE SG-MENSAGEM(01)    TO WRK-OUT-MENSA-FKTM
               END-IF

               PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                          UNTIL WRK-IND GREATER 10

             WHEN '0003'
             WHEN '0100'
                MOVE '*'                   TO WRK-OUT-ULTPAG-FKTM
                MOVE SG-MENSAGEM(63)       TO WRK-OUT-MENSA-FKTM

                PERFORM 2115-LIMPAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 10

             WHEN '0099'
               MOVE 'DB2'                  TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
                MOVE SAI5520-MSG-RETORNO   TO WRK-OUT-MENSA-FKTM
                PERFORM 2210-DEVOLVER-TELA-VARIA

           END-EVALUATE.

           COMPUTE WRK-OUT-PAGINA-FKTM = WRK-INP-PAGINA-FKTM + 1.

      *----------------------------------------------------------------*
       2260-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2270-TRATAR-ENTER                   SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                    TO WRK-INCONSIS

           PERFORM 2271-CONSISTIR-FISICA.
      *
      *----------------------------------------------------------------*
       2270-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-OPER-FKTM-X(1)    NOT NUMERIC OR
               WRK-INP-OPER-FKTM-X(1)    EQUAL SPACES
               MOVE WRK-49345            TO WRK-OUT-COMANDO-ATTR-FKTM
               MOVE 'NAO FORAM ENCONTRADOS DADOS PARA ESTE ARGUMENTO DE
      -             'PESQUISA'
                                         TO WRK-OUT-MENSA-FKTM
               GO TO 2271-99-FIM
           END-IF

           MOVE ZEROS                      TO WCT-SELEC

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 10

               IF  WRK-INP-SELEC-FKTM(WRK-IND) NOT EQUAL SPACE AND 'X'
                   MOVE '*'                TO WRK-INCONSIS
                   MOVE WRK-49369          TO
                        WRK-OUT-SELEC-ATTR-FKTM(WRK-IND)
                   MOVE SG-MENSAGEM(41)    TO WRK-OUT-MENSA-FKTM
               ELSE
                   IF  WRK-INP-SELEC-FKTM(WRK-IND) EQUAL 'X'
                       MOVE WRK-49369      TO
                            WRK-OUT-SELEC-ATTR-FKTM(WRK-IND)
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
                   MOVE SG-MENSAGEM(53)    TO WRK-OUT-MENSA-FKTM
               ELSE
                 IF  WCT-SELEC             GREATER 1
                     MOVE SG-MENSAGEM(39)  TO WRK-OUT-MENSA-FKTM
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
           MOVE WRK-INP-TMSTAMP-FKTM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15

               IF  WRK-CHAMADOR(IND-1)      EQUAL SPACES
                   MOVE 'DCOM0611'          TO WRK-CHAMADOR(IND-1)
                                               COMU-TRANSACAO
                   MOVE WRK-INP-PAGINA-FKTM TO WRK-PAGINA(IND-1)
                   MOVE 99                  TO IND-1
               END-IF
           END-PERFORM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 10

               IF  WRK-INP-SELEC-FKTM(WRK-IND) EQUAL 'X'
                   MOVE WRK-INP-OPER-FKTM(WRK-IND)(1:4)
                                    TO WRK-DANO-OPER-DESC
                   MOVE WRK-INP-OPER-FKTM(WRK-IND)(5:9)
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

           MOVE LENGTH OF WRK-OUTPUT-FKTM
                                       TO  WRK-660-LL-MENS-FKTM
                                           WRK-OUT-LL-FKTM

           MOVE LENGTH OF WRK-660-DCOMFKTM
                                       TO  WRK-660-LL-AREA-FKTM

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FKTM
                                              WRK-660-DCOMFKTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-FKTM            TO WRK-MENSAGEM.

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
           MOVE WRK-INP-TMSTAMP-FKTM       TO WRK-ENT-TIMESTAMP
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

           MOVE WRK-CONTRATO               TO ENT5520-CCONTR-LIM-DESC
           MOVE 10                         TO ENT5520-CELMTO-DESC-COML

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
                   MOVE '*'                   TO WRK-OUT-ULTPAG-FKTM
                   IF (SAI5520-QTDE-RETORNADA EQUAL ZEROS)
                       MOVE WRK-49345       TO WRK-OUT-COMANDO-ATTR-FKTM
                       MOVE SG-MENSAGEM(63)   TO WRK-OUT-MENSA-FKTM
                    ELSE
                       MOVE SG-MENSAGEM(61)   TO WRK-OUT-MENSA-FKTM
                    END-IF
                ELSE
                    MOVE SPACES            TO WRK-OUT-ULTPAG-FKTM
                    MOVE SG-MENSAGEM(61)   TO WRK-OUT-MENSA-FKTM
                END-IF

                PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 10

              WHEN '0003'
              WHEN '0100'
                MOVE WRK-49345             TO WRK-OUT-COMANDO-ATTR-FKTM
                MOVE '*'                   TO WRK-OUT-ULTPAG-FKTM
                MOVE 'NAO FORAM ENCONTRADOS DADOS PARA ESTE ARGUMENTO DE
      -              ' PESQUISA'           TO WRK-OUT-MENSA-FKTM
                                              SAI5520-MSG-RETORNO

                PERFORM 2115-LIMPAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 10

              WHEN '0099'
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                MOVE WRK-49345             TO WRK-OUT-COMANDO-ATTR-FKTM
                MOVE SAI5520-MSG-RETORNO   TO WRK-OUT-MENSA-FKTM

                PERFORM 2115-LIMPAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 10

           END-EVALUATE.

      *----------------------------------------------------------------*
       7000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO          SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0611'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0611'             TO ERR-MODULO
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
