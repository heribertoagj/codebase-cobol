      *================================================================*
      * I#BVVETN - TRAVA AUTOMATICA                                    *
      *            LAYOUT RECEPCAO AGENDA FINANCEIRA DA REDE           *
      *            FORMATACAO DO CPF NO FORMATO CIP                    *
      *----------------------------------------------------------------*
      * LRECL: 280                                                     *
      *================================================================*
       01  BVVETN-REGISTRO.
           05  BVVETN-CTRL-ARQ         PIC X(008)          VALUE SPACES.
           05  BVVETN-SEQ-REG          PIC 9(005)          VALUE ZEROS.
           05  BVVETN-SERVICO          PIC X(007)          VALUE SPACES.
           05  BVVETN-BANCO            PIC 9(003)          VALUE ZEROS.
           05  BVVETN-AGENCIA          PIC 9(005)          VALUE ZEROS.
           05  BVVETN-AGEN-DIG         PIC X(001)          VALUE SPACES.
           05  BVVETN-CONTA            PIC 9(012)          VALUE ZEROS.
           05  BVVETN-CONTA-DIG        PIC X(001)          VALUE SPACES.
           05  FILLER                  PIC X(030)          VALUE SPACES.
           05  BVVETN-BANDEIRA         PIC X(001)          VALUE SPACES.
      *         V = VISA
      *         O = OUTRAS
           05  BVVETN-SEU-NUMERO       PIC X(020)          VALUE SPACES.
           05  BVVETN-DT-PAGTO         PIC 9(008)          VALUE ZEROS.
      *         FORMATO: DDMMAAAA
           05  BVVETN-TP-MOEDA         PIC X(003)          VALUE SPACES.
           05  FILLER                  PIC X(015)          VALUE SPACES.
           05  BVVETN-VLR-PAGTO        PIC 9(013)V99       VALUE ZEROS.
           05  FILLER                  PIC X(045)          VALUE SPACES.
           05  BVVETN-CNPJ-CPF         PIC 9(014)          VALUE ZEROS.
      *         CPF  = 99999999999000 => 999999999-99 000
      *         CNPJ = 99999999999999 => 99999999/9999-99
           05  BVVETN-TP-PSSOA         PIC X(001)          VALUE SPACES.
           05  FILLER                  PIC X(046)          VALUE SPACES.
           05  BVVETN-BAND-CIP         PIC X(003)          VALUE SPACES.
           05  FILLER                  PIC X(037)          VALUE SPACES.
