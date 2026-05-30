      ******************************************************************
      *      CADASTRO DE CLIENTES - PREVISTO ANO 2000                  *
      *                                                                *
      *      ARQ. COMPRIMIDO - COM HEADER E TRAILLER                   *
      *      HEADER:   AG/RZ/CTA  =  ZEROS                             *
      *      TRAILLER: AG/RZ/CTA  =  NOVES                             *
      *                                                                *
      *      CLASSIF. AG/RZ/CTA - DSN=(MJ.AF.)CLIE.PRP.CADANOVO(0)     *
      *      CLASSIF. AG/CTA    - DSN=(MJ.AF.)CLIE.PRP.CADNAGCT(0)     *
      *                                                                *
      *    INC = I#CLIE20         LRECL = 0180          RECFM = FB     *
      ******************************************************************

       01  REG-CADACLIE.
           03  CAD-CHAVE.
               05  CAD-AGENCIA          PIC  9(05)     COMP-3.
               05  CAD-RAZAO            PIC  9(05)     COMP-3.
               05  CAD-CONTA            PIC  9(07)     COMP-3.
           03  CAD-DIG-CTA              PIC  X(01).
           03  CAD-CGC-CPF.
               05  CAD-PRINCIPAL        PIC  9(09)     COMP-3.
               05  CAD-FILIAL           PIC  9(05)     COMP-3.
               05  CAD-CTLE             PIC  9(02).
           03  CAD-NOME-CLIE            PIC  X(40).
           03  CAD-ENDERECO             PIC  X(40).
           03  CAD-CEP.
               05  CAD-NRO-CEP          PIC  9(05)     COMP-3.
               05  CAD-COMPL-CEP        PIC  9(03).
               05  CAD-COMPL-CEP-NULL   REDEFINES
                   CAD-COMPL-CEP        PIC  X(03).
           03  CAD-TELEFONE.
               05  CAD-DDD-FONE         PIC  X(04).
               05  CAD-NRO-FONE         PIC  9(09)     COMP-3.
           03  CAD-FAX.
               05  CAD-DDD-FAX          PIC  X(04).
               05  CAD-NRO-FAX          PIC  9(09)     COMP-3.
           03  CAD-POSTO-SERV           PIC  9(03)     COMP-3.
           03  CAD-RAMO-ATIV            PIC  9(05)     COMP-3.
           03  CAD-IDENTF-CTA           PIC  X(02).
           03  CAD-DT-ABERT             PIC  9(09)     COMP-3.
           03  CAD-DT-ULTMV             PIC  9(09)     COMP-3.
           03  CAD-DT-NASCI             PIC  9(09)     COMP-3.
           03  CAD-SEXO                 PIC  X(01).
           03  CAD-TP-CHEQ              PIC  X(01).
           03  CAD-TP-EXTR              PIC  X(01).
           03  CAD-RESERVA              PIC  X(30).

