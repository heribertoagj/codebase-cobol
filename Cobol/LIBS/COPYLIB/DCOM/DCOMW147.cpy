      *================================================================*
      *                    TECH MAHINDRA                               *
      *----------------------------------------------------------------*
      *    BOOK........:   DCOMW147                                    *
      *    ANALISTA....:   CEZAR VARGAS FERNANDES   - TECH MAHINDRA    *
      *    ANALISTA DDS:   RICARDO JAMMAL           - BRADESCO / GP.39 *
      *    DATA........:   07/07/2022                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   BOOK DE CONTENDO INFORMACOES DE BAIXA E     *
      *                    REATIVACOES.                                *
      *                                                                *
      *    ORGANIZACAO.:   SEQUENCIAL                                  *
      *    TAMANHO.....:   350                                         *
      *----------------------------------------------------------------*
      *    W147-TREGTO :   (1= BAIXA;               2= REATIVACAO)     *
      *    W147-MOTVBX :   (1= NORMAL;              3= FRAUDE;         *
      *                :   (4= RISCO OPERACIONAL;   5= CONCES COMERCIAL*
      *    W147-TEVENTO:   (1= PERDA OPERACIONAL;   7= ESTORNO         *
      *----------------------------------------------------------------*

       01  W147-REGISTRO.
           05  W147-TREGTO             PIC  9(001)     VALUE  ZEROS .
           05  W147-MOTVBX             PIC  9(003)     VALUE  ZEROS .
           05  W147-TEVENTO            PIC  9(001)     VALUE  ZEROS .
           05  W147-DANO-OPER-DESC     PIC  9(004)     VALUE  ZEROS .
           05  W147-NSEQ-OPER-DESC     PIC  9(009)     VALUE  ZEROS .
           05  W147-NPCELA-DESC-COML   PIC  9(005)     VALUE  ZEROS .
           05  W147-DATA-MOVIMENTO     PIC  X(010)     VALUE  SPACES.
           05  W147-VLR-DIVIDA         PIC  9(015)V99  VALUE  ZEROS .
           05  W147-CNVL-0             PIC  9(002)     VALUE  ZEROS .
           05  W147-CNVL-1             PIC  X(004)     VALUE  SPACES.
           05  W147-CNVL-2             PIC  X(006)     VALUE  SPACES.
           05  W147-CNVL-3             PIC  X(008)     VALUE  SPACES.
           05  W147-CAUSA-PERDA        PIC  9(002)     VALUE  ZEROS .
           05  W147-CRZAO-CTBIL-DEBT   PIC  X(005)     VALUE  SPACES.
           05  W147-CCTA-CTBIL-DEBT    PIC  X(004)     VALUE  SPACES.
           05  W147-CCOSIF-DEBT        PIC  X(008)     VALUE  SPACES.
           05  W147-CRZAO-CTBIL-CRED   PIC  X(005)     VALUE  SPACES.
           05  W147-CCTA-CTBIL-CRED    PIC  X(004)     VALUE  SPACES.
           05  W147-CCOSIF-CRED        PIC  X(008)     VALUE  SPACES.
           05  W147-JUSTF-HIST-TXT-1   PIC  X(067)     VALUE  SPACES.
           05  W147-JUSTF-HIST-TXT-2   PIC  X(079)     VALUE  SPACES.
           05  W147-AAAA-MM-DD-HULT    PIC  X(010)     VALUE  SPACES.
           05  W147-VLR-PRINC          PIC  9(009)V99  VALUE  ZEROS .
           05  W147-VLR-ATULZ          PIC  9(009)V99  VALUE  ZEROS .
           05  W147-AGENCIA            PIC  9(005)     VALUE  ZEROS .
           05  W147-CONTA              PIC  9(007)     VALUE  ZEROS .
           05  W147-PRODUTO            PIC  9(003)     VALUE  ZEROS .
           05  W147-SUBPRODUTO         PIC  9(003)     VALUE  ZEROS .
           05  W147-CONTRATO           PIC  9(017)     VALUE  ZEROS .
           05  W147-TIPO-PSSOA         PIC  X(001)     VALUE  SPACE .
           05  W147-CPF-CNPJ-NRO       PIC  9(009)     VALUE  ZEROS .
           05  W147-CPF-CNPJ-FLIAL     PIC  9(004)     VALUE  ZEROS .
           05  W147-CPF-CNPJ-CTRL      PIC  9(002)     VALUE  ZEROS .
           05  W147-CARTEIRA           PIC  9(005)     VALUE  ZEROS .
           05  W147-RESUM-CTBIL        PIC  9(005)     VALUE  ZEROS .
           05  FILLER                  PIC  X(005)     VALUE  SPACES.

      *================================================================*
      *    FIM DO BOOK DCOMW147                                        *
      *================================================================*
