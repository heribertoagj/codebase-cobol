      ****************************************************************
      *  CARTAO DE DEBITO - CONSULTA DE ORPAGS POR PRODUTO           *
      *  INC  -  I#BVVEIO                                            *
      *          AREA DE COMUNICACAO COM O MODULO BVVE1335           *
      *          (BVVEIO-ENT-ENTRADA=23 - BVVEIO-SAI-SAIDA=4125)     *
      ****************************************************************
       01  BVVEIO-AREA.
           05 BVVEIO-ENT-ENTRADA.
              10 BVVEIO-ENT-BANCO            PIC  9(03) COMP-3.
              10 BVVEIO-ENT-AGENCIA          PIC  9(05) COMP-3.
              10 BVVEIO-ENT-CONTA            PIC  9(13) COMP-3.
              10 BVVEIO-ENT-CPF-CNPJ         PIC  9(15) COMP-3.
              10 BVVEIO-ENT-QTDE-RESTART     PIC  9(05) COMP-3.
           05 BVVEIO-SAI-SAIDA.
              10 BVVEIO-SAI-RETORNO.
                 15 BVVEIO-SAI-COD-RETORNO   PIC  9(02).
                 15 BVVEIO-SAI-MENSAGEM      PIC  X(50).
              10 BVVEIO-SAI-CLIENTE.
                 15 BVVEIO-SAI-BANCO         PIC 9(03) COMP-3.
                 15 BVVEIO-SAI-AGENCIA       PIC 9(05) COMP-3.
                 15 BVVEIO-SAI-CONTA         PIC 9(13) COMP-3.
                 15 BVVEIO-SAI-CPF-CNPJ      PIC 9(15) COMP-3.
              10 BVVEIO-SAI-RESTART.
                 15 BVVEIO-SAI-QTDE-RESTART  PIC 9(05) COMP-3.
                 15 BVVEIO-SAI-QTDE-OCORRENCIAS
                                             PIC 9(05) COMP-3.
              10 BVVEIO-SAI-OCORRENCIAS      OCCURS  50  TIMES.
                 15 BVVEIO-SAI-COD-PRODUTO   PIC 9(05).
                 15 BVVEIO-SAI-ROTATIVO-PARCELADO
                                             PIC X(01).
                 15 BVVEIO-SAI-DESCRICAO-PRODUTO
                                             PIC X(25).
                 15 BVVEIO-SAI-PSDO-ANTEC-CREDT
                                             PIC 9(03)V9(02) COMP-3.
                 15 BVVEIO-SAI-VLR-MAX-ANTEC
                                             PIC 9(15)V9(02) COMP-3.
                 15 BVVEIO-SAI-TOT-CREDT-ESTBL
                                             PIC 9(15)V9(02) COMP-3.
                 15 BVVEIO-SAI-TOT-DEB-ESTBL
                                             PIC 9(15)V9(02) COMP-3.
                 15 BVVEIO-SAI-DISPN-ANTEC-CREDT
                                             PIC 9(15)V9(02) COMP-3.
                 15 BVVEIO-SAI-TOT-MOVTC-ANTEC
                                             PIC 9(15)V9(02) COMP-3.
              10 BVVEIO-SAI-RETORNO-DB2.
                 15 BVVEIO-SAI-COD-SQLCODE   PIC S9(04).
                 15 BVVEIO-SAI-AREA-ERRO-DB2 PIC X(107).
                 15 BVVEIO-SAI-AREA-SQLCA    PIC X(136).
