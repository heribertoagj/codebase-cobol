      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM0615.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0615                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: EMERSON ARTUR FRANCO - ALTRAN.               *
      *     DATA........: 19/05/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CONSULTA OPERACAO - DETALHES DO AVALISTA     *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMFOTM....: CONSULTA OPERACAO - DETALHES DO AVALISTA     *
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
      *     DCOM4368 - MODULO DE ACESSO                                *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0614                   CHAMA:            *
      *================================================================*
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
           '*** DCOM0615 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  IND-1                           PIC S9(09) COMP VALUE ZEROS.
       77  WRK-IND                         PIC S9(09) COMP VALUE ZEROS.
       77  WIND-SEL                        PIC S9(09) COMP VALUE ZEROS.
       77  WCT-SELEC                       PIC S9(09) COMP VALUE ZEROS.
       77  WRK-VOPER-E                     PIC ZZZ.ZZZ.ZZ9,99.
       77  WRK-TAXA-E                      PIC 99,99.
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
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                       PIC S9(08) COMP VALUE +49369.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-MODULO                  PIC  X(08)      VALUE SPACES.
           05  WRK-INCONSIS                PIC  X(01)      VALUE SPACES.
      *
           05  WRK-CNPJ-E.
               10 WRK-CCNPJ-CPF-E          PIC 9(09)       VALUE ZEROS.
               10 FILLER                   PIC X(01)       VALUE '/'.
               10 WRK-CFLIAL-CNPJ-E        PIC 9(04)       VALUE ZEROS.
               10 FILLER                   PIC X(01)       VALUE '-'.
               10 WRK-CCTRL-CNPJ-CPF-E     PIC 9(02)       VALUE ZEROS.
      *
           05  WRK-CNPJ-CONJGE.
               10 WRK-CCNPJ-CPF-CONJGE     PIC 9(09)       VALUE ZEROS.
               10 FILLER                   PIC X(01)       VALUE '-'.
               10 WRK-CCTRL-CONJGE         PIC 9(02)       VALUE ZEROS.
      *
           05  WRK-BCO-AGE-CTA.
               10 WRK-CBCO                 PIC 9(03)       VALUE ZEROS.
               10 FILLER                   PIC X(01)       VALUE '/'.
               10 WRK-CAG-BCRIA            PIC 9(05)       VALUE ZEROS.
               10 FILLER                   PIC X(01)       VALUE '/'.
               10 WRK-CCTA-BCRIA-CLI       PIC 9(13)       VALUE ZEROS.
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
      *
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
           05  WRK-COD-DEPTO               PIC  9(06)      VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER                  PIC  X(04)      VALUE SPACES.
               10  WRK-TRANSACAO           PIC  X(08)      VALUE SPACES.
               10  FILLER                  PIC  X(1988)    VALUE SPACES.
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
           '*** AREA DA TELA DCOMFOTM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-FOTM.
           05  FILLER                      PIC  X(18)      VALUE SPACES.
           05  WRK-INP-PFK-FOTM            PIC  X(01)      VALUE SPACES.
           05  WRK-INP-SENHAS-FOTM         PIC  X(37)      VALUE SPACES.
           05  WRK-INP-COMANDO-FOTM        PIC  X(68)      VALUE SPACES.
           05  WRK-INP-DADOS-FOTM.
               10  WRK-INP-TMSTAMP-FOTM    PIC  X(26)      VALUE SPACES.
      *
               10  WRK-INP-NCLIE-FOTM      PIC  X(40)      VALUE SPACE.
               10  WRK-INP-CCNPJ-FOTM      PIC  X(19)      VALUE SPACE.
               10  WRK-INP-CAGCT-FOTM      PIC  X(23)      VALUE SPACE.
               10  WRK-INP-CPROD-FOTM-X.
                   15  WRK-INP-CPROD-FOTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-INP-NPROD-FOTM      PIC  X(12)      VALUE SPACE.
               10  WRK-INP-CSUBP-FOTM-X.
                   15  WRK-INP-CSUBP-FOTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-INP-NSUBP-FOTM      PIC  X(10)      VALUE SPACE.
               10  WRK-INP-COPER-FOTM-X.
                   15  WRK-INP-COPER-FOTM  PIC  9(13)      VALUE ZEROS.
               10  WRK-INP-NSITU-FOTM      PIC  X(15)      VALUE SPACE.
               10  WRK-INP-NSIT2-FOTM      PIC  X(09)      VALUE SPACES.
               10  WRK-INP-CSITU-FOTM-X.
                   15  WRK-INP-CSITU-FOTM  PIC  9(10)      VALUE ZEROS.
               10  WRK-INP-CNPJ2-FOTM      PIC  X(17)      VALUE SPACES.
               10  WRK-INP-NOME2-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-INP-CODRG-FOTM      PIC  X(15)      VALUE SPACES.
               10  WRK-INP-CDCEP-FOTM      PIC  X(09)      VALUE SPACES.
               10  WRK-INP-ENDER-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-INP-BAIRR-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-INP-CIDAD-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-INP-UF-FOTM         PIC  X(02)      VALUE SPACES.
               10  WRK-INP-AUTOR-FOTM      PIC  X(01)      VALUE SPACES.
               10  WRK-INP-CNPJ3-FOTM      PIC  X(17)      VALUE SPACES.
               10  WRK-INP-NOME3-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-INP-CDRG3-FOTM      PIC  X(15)      VALUE SPACES.
               10  WRK-INP-SALVA-FOTM      PIC  X(79)      VALUE SPACES.
           05  WRK-INP-MENSA-FOTM          PIC  X(79)      VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFOTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-FOTM.
           05  WRK-OUT-LL-FOTM             PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-ZZ-FOTM             PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FOTM         PIC  X(37)      VALUE SPACES.
           05  WRK-OUT-COMANDO-FOTM        PIC  X(68)      VALUE SPACES.
           05  WRK-OUT-DADOS-FOTM.
               10  WRK-OUT-TMSTAMP-FOTM    PIC  X(26)      VALUE SPACES.
      *
               10  WRK-OUT-NCLIE-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-OUT-CCNPJ-FOTM      PIC  X(19)      VALUE SPACES.
               10  WRK-OUT-CAGCT-FOTM      PIC  X(23)      VALUE SPACES.
      *
               10  WRK-OUT-CPROD-FOTM-X.
                   15  WRK-OUT-CPROD-FOTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-OUT-NPROD-FOTM      PIC  X(12)      VALUE SPACES.
      *
               10  WRK-OUT-CSUBP-FOTM-X.
                   15  WRK-OUT-CSUBP-FOTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-OUT-NSUBP-FOTM      PIC  X(10)      VALUE SPACES.
      *
               10  WRK-OUT-COPER-FOTM-X.
                   15  WRK-OUT-COPER-FOTM  PIC  9(13)      VALUE ZEROS.
      *
               10  WRK-OUT-NSITU-FOTM      PIC  X(15)      VALUE SPACES.
               10  WRK-OUT-NSIT2-FOTM      PIC  X(09)      VALUE SPACES.
               10  WRK-OUT-CSITU-FOTM-X.
                   15  WRK-OUT-CSITU-FOTM  PIC  9(10)      VALUE ZEROS.
      *
               10  WRK-OUT-CNPJ2-FOTM      PIC  X(17)      VALUE SPACES.
               10  WRK-OUT-NOME2-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-OUT-CODRG-FOTM      PIC  X(15)      VALUE SPACES.
               10  WRK-OUT-CDCEP-FOTM      PIC  X(09)      VALUE SPACES.
               10  WRK-OUT-ENDER-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-OUT-BAIRR-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-OUT-CIDAD-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-OUT-UF-FOTM         PIC  X(02)      VALUE SPACES.
               10  WRK-OUT-AUTOR-FOTM      PIC  X(01)      VALUE SPACES.
               10  WRK-OUT-CNPJ3-FOTM      PIC  X(17)      VALUE SPACES.
               10  WRK-OUT-NOME3-FOTM      PIC  X(40)      VALUE SPACES.
               10  WRK-OUT-CDRG3-FOTM      PIC  X(15)      VALUE SPACES.
      *
               10  WRK-OUT-SALVA-FOTM      PIC  X(79)      VALUE SPACES.
      *
           05  WRK-OUT-MENSA-FOTM          PIC  X(79)      VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFOTM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMFOTM.
           05  WRK-660-LL-AREA-FOTM        PIC  9(04) COMP VALUE 0000.
           05  WRK-660-LL-MENS-FOTM        PIC  9(04) COMP VALUE 0000.
           05  WRK-660-SENHAS-FOTM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FOTM        PIC  9(04) COMP VALUE 0068.
           05  WRK-660-TMSTAMP-FOTM        PIC  9(04) COMP VALUE 0026.
           05  WRK-660-NCLIE-FOTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-CCNPJ-FOTM          PIC  9(04) COMP VALUE 0019.
           05  WRK-660-CAGCT-FOTM          PIC  9(04) COMP VALUE 0023.
           05  WRK-660-CPROD-FOTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-NPROD-FOTM          PIC  9(04) COMP VALUE 0012.
           05  WRK-660-CSUBP-FOTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-NSUBP-FOTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-660-COPER-FOTM          PIC  9(04) COMP VALUE 0013.
           05  WRK-660-NSITU-FOTM          PIC  9(04) COMP VALUE 0015.
           05  WRK-660-NSIT2-FOTM          PIC  9(04) COMP VALUE 0009.
           05  WRK-660-CSITU-FOTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-660-CNPJ2-FOTM          PIC  9(04) COMP VALUE 0017.
           05  WRK-660-NOME2-FOTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-CODRG-FOTM          PIC  9(04) COMP VALUE 0015.
           05  WRK-660-CDCEP-FOTM          PIC  9(04) COMP VALUE 0009.
           05  WRK-660-ENDER-FOTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-BAIRR-FOTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-CIDAD-FOTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-UF-FOTM             PIC  9(04) COMP VALUE 0002.
           05  WRK-660-AUTOR-FOTM          PIC  9(04) COMP VALUE 0001.
           05  WRK-660-CNPJ3-FOTM          PIC  9(04) COMP VALUE 0017.
           05  WRK-660-NOME3-FOTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-CDRG3-FOTM          PIC  9(04) COMP VALUE 0015.
           05  WRK-660-SALVA-FOTM          PIC  9(04) COMP VALUE 0079.
           05  WRK-660-MENSA-FOTM          PIC  9(04) COMP VALUE 0079.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA PARA COMUNICACAO MODULO DCOM4368 ***'.
      *----------------------------------------------------------------*
       01  ENTRADA-ROT4368.
           05  ENT4368-HEADER.
               10  ENT4368-COD-RETORNO             PIC  X(04).
               10  ENT4368-MSG-RETORNO             PIC  X(79).
               10  ENT4368-RESTART                 PIC  9(05).
               10  ENT4368-FLAG                    PIC  X(01).
               10  ENT4368-CFUNC-BDSCO             PIC  9(09).
               10  ENT4368-CTERM                   PIC  X(08).
           05  ENT4368-DADOS.
               10  ENT4368-DANO-OPER-DESC          PIC  9(04).
               10  ENT4368-NSEQ-OPER-DESC          PIC  9(09).
FS2511*               10  ENT4368-CCNPJ-CPF               PIC  9(09).
FS2511                10  ENT4368-CCNPJ-CPF               PIC  X(09).
FS2511*               10  ENT4368-CFLIAL-CNPJ             PIC  9(04).
FS2511                10  ENT4368-CFLIAL-CNPJ             PIC  X(04).
               10  ENT4368-CCTRL-CNPJ-CPF          PIC  9(02).

      *--- AREA DE SAIDA   DO ROTEADOR --> TAM. 1618 BYTES          ---*
       01  SAIDA-ROT4368.
           05  SAI4368-HEADER.
               10  SAI4368-COD-RETORNO             PIC  X(04).
               10  SAI4368-MSG-RETORNO             PIC  X(79).
               10  SAI4368-RESTART                 PIC  9(05).
               10  SAI4368-FLAG                    PIC  X(01).
           05  SAI4368-DADOS.
               10 SAI4368-AVALISTA.
                  15  SAI4368-CDOCTO-ID            PIC X(15).
                  15  SAI4368-IPSSOA-DESC-COML     PIC X(60).
                  15  SAI4368-CINDCD-TPO-PSSOA     PIC X(01).
FS2511*                  15  SAI4368-CCNPJ-CPF            PIC 9(09).
FS2511                   15  SAI4368-CCNPJ-CPF            PIC X(09).
FS2511*                  15  SAI4368-CFLIAL-CNPJ          PIC 9(04).
FS2511                   15  SAI4368-CFLIAL-CNPJ          PIC X(04).
                  15  SAI4368-CCTRL-CNPJ-CPF       PIC 9(02).
              10  SAI4368-ENDR-AVALISTA.
                  15  SAI4368-ELOGDR               PIC X(40).
                  15  SAI4368-IBAIRRO              PIC X(20).
                  15  SAI4368-CID-ELOGDR           PIC X(40).
                  15  SAI4368-CSGL-UF              PIC X(02).
                  15  SAI4368-CCEP                 PIC X(05).
                  15  SAI4368-CCEP-COMPL           PIC X(03).
              10  SAI4368-CONJG.
                  15  SAI4368-CBCO                 PIC 9(03).
                  15  SAI4368-CAG-BCRIA            PIC X(05).
                  15  SAI4368-CCTA-BCRIA-CLI       PIC 9(13).
                  15  SAI4368-CINDCD-CONJG-AUTRZ   PIC X(01).
                  15  SAI4368-IPSSOA-CONJG         PIC X(40).
                  15  SAI4368-CCPF-CONJG           PIC 9(09).
                  15  SAI4368-CCTRL-CPF-CONJG      PIC 9(02).
                  15  SAI4368-CDOCTO-ID-CONJ       PIC X(15).
          05  SAI4368-CONSISTENCIA REDEFINES SAI4368-DADOS.
              10  SAI4368-TAMANHO-ERRO             PIC 9(01).
              10  SAI4368-TABELA-ERROS    OCCURS   18  TIMES.
                  15  SAI4368-CAMPOS-ERRO          PIC 9(01).
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
           '*** DCOM0615 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                     TO WRK-OUT-DADOS-FOTM
                                              WRK-OUT-SENHAS-FOTM
                                              WRK-OUT-COMANDO-FOTM
                                              COMU-DADOS.

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

           MOVE 'DCOMFOTM'                 TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0615'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFOTM
           END-IF.

           IF  WRK-TELA                    EQUAL 'DCOMFOTM'
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
           MOVE COMU-TIMESTAMP             TO WRK-INP-TMSTAMP-FOTM
                                              WRK-OUT-TMSTAMP-FOTM
                                              WRK-ENT-TIMESTAMP.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL


           MOVE WRK-DANO-OPER-DESC   TO WRK-OUT-COPER-FOTM-X(1:4).
           MOVE WRK-NSEQ-OPER-DESC   TO WRK-OUT-COPER-FOTM-X(5:9).
           MOVE WRK-NOMECLI          TO WRK-OUT-NCLIE-FOTM.
           MOVE WRK-CNPJCPF          TO WRK-OUT-CCNPJ-FOTM.
           STRING  '237/' WRK-AGENCIA '/'
                   WRK-CONTA         DELIMITED BY SIZE
                                     INTO WRK-OUT-CAGCT-FOTM

           MOVE WRK-SITU1                  TO WRK-OUT-NSITU-FOTM.
           MOVE WRK-SITU2                  TO WRK-OUT-NSIT2-FOTM.
           MOVE WRK-SITU3                  TO WRK-OUT-CSITU-FOTM-X.

           MOVE WRK-CCNPJ-AVAL             TO WRK-CCNPJ-CPF-E.
           MOVE WRK-FILIAL-AVAL            TO WRK-CFLIAL-CNPJ-E.
           MOVE WRK-CCTRL-AVAL             TO WRK-CCTRL-CNPJ-CPF-E.

           MOVE WRK-CNPJ-E                 TO WRK-OUT-CNPJ2-FOTM

           MOVE WRK-PRODUTO                TO WRK-OUT-CPROD-FOTM.
           MOVE WRK-DESC-PRODUTO           TO WRK-OUT-NPROD-FOTM.
           MOVE WRK-SUBPRODUTO             TO WRK-OUT-CSUBP-FOTM.
           MOVE WRK-DESC-SPROD             TO WRK-OUT-NSUBP-FOTM.

           INITIALIZE  ENTRADA-ROT4368
                       SAIDA-ROT4368

           PERFORM 7000-ACESSAR-DCOM4368.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0615'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FOTM       TO WRK-ENT-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           IF  WRK-INCONSIS                EQUAL SPACES
               MOVE 'CONSULTA EFETUADA COM SUCESSO, TECLE <PF3> PARA RET
      -             'ORNAR'                TO WRK-OUT-MENSA-FOTM
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
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
      *     FORMATACAO DOS DADOS DA TELA                               *
      *----------------------------------------------------------------*
       2110-MONTAR-TELA                    SECTION.
      *----------------------------------------------------------------*

           MOVE SAI4368-IPSSOA-DESC-COML   TO WRK-OUT-NOME2-FOTM
           MOVE SAI4368-CDOCTO-ID          TO WRK-OUT-CODRG-FOTM
           MOVE SAI4368-CCEP               TO WRK-OUT-CDCEP-FOTM(1:5)
           MOVE '-'                        TO WRK-OUT-CDCEP-FOTM(6:1)
           MOVE SAI4368-CCEP-COMPL         TO WRK-OUT-CDCEP-FOTM(7:3)
           MOVE SAI4368-ELOGDR             TO WRK-OUT-ENDER-FOTM
           MOVE SAI4368-IBAIRRO            TO WRK-OUT-BAIRR-FOTM
           MOVE SAI4368-CID-ELOGDR         TO WRK-OUT-CIDAD-FOTM
           MOVE SAI4368-CSGL-UF            TO WRK-OUT-UF-FOTM
           MOVE SAI4368-CINDCD-CONJG-AUTRZ TO WRK-OUT-AUTOR-FOTM

           IF SAI4368-CINDCD-CONJG-AUTRZ    EQUAL 'S'
               MOVE SAI4368-CCPF-CONJG      TO WRK-CCNPJ-CPF-CONJGE
               MOVE SAI4368-CCTRL-CPF-CONJG TO WRK-CCTRL-CONJGE
               MOVE WRK-CNPJ-CONJGE         TO WRK-OUT-CNPJ3-FOTM
           ELSE
               MOVE SPACES                  TO WRK-OUT-CNPJ3-FOTM
           END-IF.


           MOVE SAI4368-IPSSOA-CONJG       TO WRK-OUT-NOME3-FOTM
           MOVE SAI4368-CDOCTO-ID-CONJ     TO WRK-OUT-CDRG3-FOTM

           MOVE SG-MENSAGEM(06)            TO WRK-OUT-MENSA-FOTM.

      *----------------------------------------------------------------*
       2110-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     PROCESSAR DCOMFOTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFOTM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FOTM

           PERFORM 2205-DEVOLVER-TELA

           EVALUATE TRUE
             WHEN WRK-INP-PFK-FOTM         EQUAL 'H' OR
                   (WRK-INP-PFK-FOTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FOTM   EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-FOTM TO  WRK-OUT-MENSA-FOTM

             WHEN WRK-INP-PFK-FOTM         EQUAL '3' OR
                   (WRK-INP-PFK-FOTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FOTM   EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FOTM       EQUAL 'X' OR
                   (WRK-INP-PFK-FOTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FOTM   EQUAL 'PFK05')
                  PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-FOTM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FOTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FOTM   EQUAL 'PFK10')
                  PERFORM 2240-MENU-DCOM

               WHEN
                   (WRK-INP-PFK-FOTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FOTM   EQUAL SPACES )
                   MOVE 'CONSULTA EFETUADA COM SUCESSO, TECLE <PF3> PARA
      -                 ' RETORNAR'        TO WRK-OUT-MENSA-FOTM

               WHEN OTHER
                   MOVE SG-MENSAGEM(38)    TO  WRK-OUT-MENSA-FOTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     DEVOLVER TELA                                              *
      *----------------------------------------------------------------*
       2205-DEVOLVER-TELA                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TMSTAMP-FOTM       TO WRK-OUT-TMSTAMP-FOTM

           MOVE WRK-INP-NCLIE-FOTM         TO WRK-OUT-NCLIE-FOTM
           MOVE WRK-INP-CCNPJ-FOTM         TO WRK-OUT-CCNPJ-FOTM
           MOVE WRK-INP-CAGCT-FOTM         TO WRK-OUT-CAGCT-FOTM
           MOVE WRK-INP-CPROD-FOTM-X       TO WRK-OUT-CPROD-FOTM-X
           MOVE WRK-INP-NPROD-FOTM         TO WRK-OUT-NPROD-FOTM
           MOVE WRK-INP-CSUBP-FOTM-X       TO WRK-OUT-CSUBP-FOTM-X
           MOVE WRK-INP-NSUBP-FOTM         TO WRK-OUT-NSUBP-FOTM
           MOVE WRK-INP-COPER-FOTM-X       TO WRK-OUT-COPER-FOTM-X
           MOVE WRK-INP-NSITU-FOTM         TO WRK-OUT-NSITU-FOTM
           MOVE WRK-INP-NSIT2-FOTM         TO WRK-OUT-NSIT2-FOTM
           MOVE WRK-INP-CSITU-FOTM-X       TO WRK-OUT-CSITU-FOTM-X
           MOVE WRK-INP-SALVA-FOTM         TO WRK-OUT-SALVA-FOTM
           MOVE WRK-INP-CNPJ2-FOTM         TO WRK-OUT-CNPJ2-FOTM
           MOVE WRK-INP-NOME2-FOTM         TO WRK-OUT-NOME2-FOTM
           MOVE WRK-INP-CODRG-FOTM         TO WRK-OUT-CODRG-FOTM
           MOVE WRK-INP-CDCEP-FOTM         TO WRK-OUT-CDCEP-FOTM
           MOVE WRK-INP-ENDER-FOTM         TO WRK-OUT-ENDER-FOTM
           MOVE WRK-INP-BAIRR-FOTM         TO WRK-OUT-BAIRR-FOTM
           MOVE WRK-INP-CIDAD-FOTM         TO WRK-OUT-CIDAD-FOTM
           MOVE WRK-INP-UF-FOTM            TO WRK-OUT-UF-FOTM
           MOVE WRK-INP-AUTOR-FOTM         TO WRK-OUT-AUTOR-FOTM
           MOVE WRK-INP-CNPJ3-FOTM         TO WRK-OUT-CNPJ3-FOTM
           MOVE WRK-INP-NOME3-FOTM         TO WRK-OUT-NOME3-FOTM
           MOVE WRK-INP-CDRG3-FOTM         TO WRK-OUT-CDRG3-FOTM

           MOVE WRK-INP-MENSA-FOTM         TO WRK-OUT-MENSA-FOTM.

      *----------------------------------------------------------------*
       2205-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM0103                   *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-IMS
           MOVE SPACES                     TO COMU-DADOS

           MOVE WRK-INP-TMSTAMP-FOTM       TO WRK-ENT-TIMESTAMP
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               MOVE WRK-CHAMADOR(IND-1)    TO WRK-TELA
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FOTM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE LENGTH COMU-AREA-IMS       TO COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0615'                 TO COMU-TRANSACAO
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

           MOVE WRK-INP-TMSTAMP-FOTM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0615'                 TO WRK-TELA
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

           MOVE WRK-INP-TMSTAMP-FOTM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0615'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE 'A'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF  WRK-OUTPUT-FOTM
                                       TO  WRK-660-LL-MENS-FOTM
                                           WRK-OUT-LL-FOTM

           MOVE LENGTH                 OF  WRK-660-DCOMFOTM
                                       TO  WRK-660-LL-AREA-FOTM

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FOTM
                                              WRK-660-DCOMFOTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-FOTM            TO WRK-MENSAGEM.

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
       7000-ACESSAR-DCOM4368               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                         ENTRADA-ROT4368
                                              SAIDA-ROT4368

           MOVE ZEROS                      TO ENT4368-FLAG
           MOVE ZEROS                      TO ENT4368-RESTART
           MOVE WRK-COD-USER               TO ENT4368-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT4368-CTERM
           MOVE WRK-DANO-OPER-DESC         TO ENT4368-DANO-OPER-DESC
           MOVE WRK-NSEQ-OPER-DESC         TO ENT4368-NSEQ-OPER-DESC
           MOVE WRK-CCNPJ-AVAL             TO ENT4368-CCNPJ-CPF
           MOVE WRK-FILIAL-AVAL            TO ENT4368-CFLIAL-CNPJ
           MOVE WRK-CCTRL-AVAL             TO ENT4368-CCTRL-CNPJ-CPF

           MOVE 'DCOM4368'                 TO WRK-MODULO

           CALL WRK-MODULO             USING  ENTRADA-ROT4368
                                              SAIDA-ROT4368
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI4368-COD-RETORNO
              WHEN '0000'
                PERFORM 2110-MONTAR-TELA

              WHEN '0099'
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE SAI4368-MSG-RETORNO TO WRK-OUT-MENSA-FOTM
                   MOVE '*'                 TO WRK-INCONSIS

           END-EVALUATE .

      *----------------------------------------------------------------*
       7000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO          SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0615'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0615'             TO ERR-MODULO
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
