      ******************************************************************
      *      CADASTRO DE CLIENTES - PREVISTO ANO 2000                  *
      *                                                                *
      *      HEADER:   AG/RZ/CTA  =  ZEROS                             *
      *      TRAILLER: AG/RZ/CTA  =  NOVES                             *
      *                                                                *
      *    COPIA DA INC I#DEVE17                                       *
      *                                                                *
      *    INC = I#DEVEN7         LRECL = 0185          RECFM = FB     *
      ******************************************************************
4SYS01*    ULTIMA ATUALIZACAO                                          *
4SYS01*                                                                *
4SYS01*    INCLUSAO DE CNPJ E OU CPF ALFANUMERICO                      *
4SYS01*                                                                *
4SYS01*    EMPRESA - FOURSYS                                           *
4SYS01*    DATA    - SET/2025                                          *
4SYS01*    CLONE DO BOOK I#DEVE17  - INCLUSAO DE CAMPO ALFANUMERICO    *
4SYS01*----------------------------------------------------------------*

       01  REG-DEVE17.
           03  DEVE17-CHAVE.
               05  DEVE17-BANCO            PIC  9(03)     COMP-3.
               05  DEVE17-AGENCIA          PIC  9(05)     COMP-3.
               05  DEVE17-RAZAO            PIC  9(05)     COMP-3.
               05  DEVE17-CONTA            PIC  9(12)     COMP-3.
               05  DEVE17-DIG-CTA          PIC  X(01).
           03  DEVE17-CGC-CPF.
               05  DEVE17-PRINCIPAL        PIC  9(09)     COMP-3.
               05  DEVE17-FILIAL           PIC  9(05)     COMP-3.
               05  DEVE17-CTLE             PIC  9(02).
           03  DEVE17-NOME-CLIE            PIC  X(40).
           03  DEVE17-ENDERECO             PIC  X(40).
           03  DEVE17-CEP.
               05  DEVE17-NRO-CEP          PIC  9(05)     COMP-3.
               05  DEVE17-COMPL-CEP        PIC  9(03).
               05  DEVE17-COMPL-CEP-NULL   REDEFINES
                   DEVE17-COMPL-CEP        PIC  X(03).
           03  DEVE17-TELEFONE.
               05  DEVE17-DDD-FONE         PIC  X(04).
               05  DEVE17-NRO-FONE         PIC  9(09)     COMP-3.
           03  DEVE17-FAX.
               05  DEVE17-DDD-FAX          PIC  X(04).
               05  DEVE17-NRO-FAX          PIC  9(09)     COMP-3.
           03  DEVE17-POSTO-SERV           PIC  9(03)     COMP-3.
           03  DEVE17-RAMO-ATIV            PIC  9(05)     COMP-3.
           03  DEVE17-IDENTF-CTA           PIC  X(02).
           03  DEVE17-DT-ABERT             PIC  9(09)     COMP-3.
           03  DEVE17-DT-ULTMV             PIC  9(09)     COMP-3.
           03  DEVE17-DT-NASCI             PIC  9(09)     COMP-3.
           03  DEVE17-SEXO                 PIC  X(01).
           03  DEVE17-TP-PESSOA            PIC  X(01).
           03  DEVE17-TP-SITUACAO          PIC  X(01).
4SYS01     03  DEVE17-CGC-CPF-ST.
4SYS01         05  DEVE17-PRINCIPAL-ST     PIC  X(09).
4SYS01         05  DEVE17-FILIAL-ST        PIC  X(05).
4SYS01         05  DEVE17-CTLE-ST          PIC  9(02).
4SYS01     03  DEVE17-RESERVA              PIC  X(14).
