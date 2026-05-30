
      *---------------------------------------------------------------*
      *   INC  'I#BVVE04' -                    LRECL = 230            *
      *                                                               *
      *   ARQUIVO DE OCORRENCIAS                                      *
      *                                                               *
      *   PGM:   BVVE0495                                             *
      *                                                               *
      *---------------------------------------------------------------*

       01   REG-OCORRENCIA.
            03  FILLER                         PIC XX.
      *     POS  3
            03  REG-BVVE04-AGENCIA             PIC 9(05)      COMP-3.
      *     POS  6
            03  REG-BVVE04-RAZAO               PIC 9(05)      COMP-3.
      *     POS  9
            03  REG-BVVE04-CONTA               PIC 9(07)      COMP-3.
            03  FILLER                         PIC X(12).
            03  REG-BVVE04-NOME                PIC X(35).
      *     POS  60
            03  REG-BVVE04-ENDERECO            PIC X(35).
      *     POS  95
            03  REG-BVVE04-CEP                 PIC 9(05).
            03  FILLER                         PIC X(100).
      *     POS  200
            03  REG-BVVE04-DTAMOVTO            PIC 9(09)      COMP-3.
            03  FILLER                         PIC X(26).


