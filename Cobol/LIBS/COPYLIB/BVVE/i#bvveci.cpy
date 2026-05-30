      ******************************************************************
      *                                                                *
      *   AREA     : I#BVVECI                                          *
      *   TAMANHO  : 1920                                              *
      *   OBJETIVO : LAY-OUT MENSAGEM ONLINE - CIELO                   *
      *                                                                *
      ******************************************************************
       01  MSG-ENVIO-CIELO.
           03  ENV-COD-MENSAGEM              PIC 9(03).
      *        035 - QUANDO VEM DO BANCO
      *        135 - NO RETORNO AOS BANCOS
           03  ENV-INST-FINANCEIRA           PIC 9(04).
           03  ENV-TERMINAL                  PIC X(04).
           03  ENV-COD-RETORNO-GERAL         PIC 9(02).
      *        00 - OK
      *        99 - ERRO
           03  ENV-BANCO-ORIGEM              PIC 9(04).
           03  ENV-BANCO-ORIGEM-COD-RET      PIC 9(02).
           03  ENV-ESTAB                     PIC 9(10).
           03  ENV-ESTAB-COD-RET             PIC 9(02).
           03  ENV-CNPJ                      PIC 9(17).
           03  ENV-CNPJ-COD-RET              PIC 9(02).
           03  ENV-TIPO-DOMIC                PIC 9(02).
      *        0 - VISA E VISA VALE
      *        1 - VISA
      *        2 - VISA VALE
      *        5 - MASTERCARD
           03  ENV-TIPO-DOMIC-COD-RET        PIC 9(02).
           03  ENV-TIPO-CONTA                PIC X(01).
      *        0 - JURIDICA
      *        1 - FISICA
           03  ENV-TIPO-CONTA-COD-RET        PIC 9(02).
           03  ENV-BANCO                     PIC 9(04).
           03  ENV-BANCO-COD-RET             PIC 9(02).
           03  ENV-AGENCIA                   PIC 9(05).
           03  ENV-AGENCIA-COD-RET           PIC 9(02).
           03  ENV-CONTA                     PIC X(14).
           03  ENV-CONTA-COD-RET             PIC 9(02).
           03  ENV-SOLICITANTE               PIC X(20).
      *        IDENTIFICADOR DO USUARIO DO BANCO QUE SOLICITOU A
      *        ALTERACAO
           03  ENV-SOLICITANTE-COD-RET       PIC 9(02).
           03  FILLER                        PIC X(1812).
