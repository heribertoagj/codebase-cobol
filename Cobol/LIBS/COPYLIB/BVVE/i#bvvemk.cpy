      ******************************************************************
      *  PROJETO DE TRAVA MASSIVA - CIP                                *
      *  I#BVVEMK - INCLUSOES DE TRAVA EM MASSA                        *
      ******************************************************************
       01  BVVEMK-INCLUSAO-MASSA.
            03  BVVEMK-TP-REG              PIC X(01).
            03  BVVEMK-HEADER.
                05  BVVEMK-HD-DATA         PIC X(10).
                05  BVVEMK-HD-HORA         PIC X(08).
                05  BVVEMK-HD-ROTINA       PIC X(04).
                05  BVVEMK-HD-ORIGEM       PIC X(01).
                05  FILLER                 PIC X(56).
            03  BVVEMK-DETALHE REDEFINES   BVVEMK-HEADER.
                05  BVVEMK-DT-CNPJ.
                    07  BVVEMK-DT-CNPJN    PIC 9(15).
                05  FILLER     REDEFINES   BVVEMK-DT-CNPJ.
                    07  BVVEMK-DT-CPF.
                        09  BVVEMK-DT-CPFN PIC 9(11).
                    07  FILLER             PIC X(04).
                05  BVVEMK-DT-TP-PSSOA     PIC X(01).
                05  BVVEMK-DT-TRAVA-FIL    PIC X(01).
                05  BVVEMK-DT-AGENCIA.
                    07 BVVEMK-DT-AGE-N     PIC 9(05).
                05  BVVEMK-DT-CONTA.
                    07 BVVEMK-DT-CONTA-N   PIC 9(07).
                05  BVVEMK-DT-BANDEIRA     PIC X(03).
                05  BVVEMK-DT-PRAZO.
                    07 BVVEMK-DT-PRAZON    PIC 9(04).
                05  FILLER                 PIC X(43).
            03  BVVEMK-TRAILLER REDEFINES  BVVEMK-HEADER.
                05  BVVEMK-QTD-REG-TR      PIC 9(09).
                05  FILLER                 PIC X(70).
