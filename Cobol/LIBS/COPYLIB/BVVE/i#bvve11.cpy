      ******************************************************************
      *                                                                *
      *  AREA DE COMUNICACAO COM A TRANSACAO XXXX - PROGRAMA BVVE0351  *
      *                                                                *
      *  LRECL=0347                                                    *
      ******************************************************************

       01  COMUAREA-BVVE0531.
           05  COMU-HPD-HEADER.
               07  COMU-HPD-BLOCO-A.
                   09  COMU-HPD-TRX           PIC  X(04).
                   09  COMU-HPD-ID            PIC  X(03).
                   09  COMU-HPD-NPERIF        PIC  X(08).
                   09  COMU-HPD-PERIF-ANT     PIC  X(01).
                   09  COMU-HPD-LU-RESP       PIC  9(01).

               07  COMU-HPD-BLOCO-B.
                   09  COMU-HPD-AG-ORI        PIC  9(04).
                   09  COMU-HPD-SE-PERIF      PIC  X(06).
                   09  COMU-HPD-AG-DST        PIC  9(04).
                   09  COMU-HPD-ANULAC        PIC  X(20).
                   09  COMU-HPD-CD-PERIF      PIC  9(03).
                   09  COMU-HPD-CTRL-TX       PIC  X(02).
                   09  COMU-HPD-RCODE-1       PIC  X(01).
                   09  COMU-HPD-RESERVA       PIC  X(23).

               07  COMU-HPD-BLOCO-C.
                   09  COMU-HPD-FN            PIC  X(01).
                   09  COMU-HPD-AG-FILLER     PIC  X(09).
                   09  COMU-HPD-VSR-APLIC     PIC  X(03).
                   09  COMU-HPD-RCODE         PIC  X(01).
                   09  COMU-HPD-MSG-COMPLETA  PIC  X(80).
                   09  COMU-HPD-MSG-RESUMIDA  PIC  X(20).
                   09  COMU-HPD-CD-MSG        PIC  9(07).
                   09  COMU-HPD-SERVICO       PIC  X(04).
                   09  COMU-HPD-TP-EXEC       PIC  X(01).
                   09  COMU-HPD-SYSID-O       PIC  X(04).

           05  COMU-ENTRADA.
               07  COMU-ENTR-AGENCOLD         PIC  9(05).
               07  COMU-ENTR-CONTAOLD         PIC  9(07).
               07  COMU-ENTR-DGCTAOLD         PIC  X(01).
               07  COMU-ENTR-COD-TDB          PIC  X(02).
               07  COMU-ENTR-AGENCNEW         PIC  9(05).
               07  COMU-ENTR-CONTANEW         PIC  9(07).
               07  COMU-ENTR-DGCTANEW         PIC  X(01).
               07  COMU-ENTR-ESTAB            PIC  9(10).
               07  COMU-ENTR-CNPJ             PIC  9(17).

           05  COMU-RETORNO.
               07  COMU-RETO-COD-ERRO         PIC  9(02).
               07  COMU-RETO-MENSAGEM         PIC  X(80).
