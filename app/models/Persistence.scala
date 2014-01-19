package models

class Persistence {

}

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity
import org.squeryl.dsl.{OneToMany, ManyToOne}

object Database extends Schema {
  
  val usersTable: Table[User] = table[User]("users")

  on(usersTable) {
    u => declare { u.id is(autoIncremented("users_id_seq")) }
  }
}